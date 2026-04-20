(in-package :shakespeare2)

;;; ===========================================================================
;;; HTTP + WebSocket handlers and server entry point.
;;;
;;; The auth gate is compiled in only when the :shakespeare2/auth feature is
;;; present (SHAKESPEARE2_AUTH=true at build time). When absent, every path
;;; is public and the auth symbols are not referenced.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; WebSocket Origin validation
;;;
;;; Without this check any webpage can open a WebSocket to our server
;;; (cross-site WebSocket hijacking). *allowed-origins* is an explicit allowlist
;;; of scheme+host(+port); an empty list falls back to same-origin.
;;; ---------------------------------------------------------------------------

(defun same-origin-p (origin host)
  "True if ORIGIN's authority matches HOST. Strips scheme and any path."
  (when (and origin host)
    (let* ((after-scheme (or (search "://" origin) -3))
           (authority-start (+ after-scheme 3))
           (path-start (or (position #\/ origin :start authority-start)
                           (length origin)))
           (authority (subseq origin authority-start path-start)))
      (string= authority host))))

(defun origin-allowed-p (request)
  "Decide whether the REQUEST's Origin header is acceptable for a WS upgrade.
   Host and scheme are case-insensitive per RFC 3986, so we compare lowercased
   strings; *allowed-origins* is already lowercased at config load time."
  (let ((origin (get-header request "origin")))
    (cond
      ;; No Origin header — browsers always send one for WS; treat absence as
      ;; a non-browser client. Permit (nginx, curl for testing, etc.).
      ((null origin) t)
      (t (let ((norm (string-downcase origin)))
           (if *allowed-origins*
               ;; Explicit allowlist wins when configured.
               (member norm *allowed-origins* :test #'string=)
               ;; Fallback: same-origin check against the Host header.
               (let ((host (get-header request "host")))
                 (and host (same-origin-p norm (string-downcase host))))))))))

;;; ---------------------------------------------------------------------------
;;; Request routing
;;; ---------------------------------------------------------------------------

(defun healthz (request)
  (declare (ignore request))
  (make-text-response 200 "ok"))

#-shakespeare2/auth
(defun handle-request (request)
  "Public build — no auth gate."
  (route-request request))

#+shakespeare2/auth
(defun handle-request (request)
  "Authed build — gate non-public paths behind a live session."
  (let ((path (http-request-path request)))
    (cond
      ;; /admin/* uses its own X-Admin-Token auth, not the session gate.
      ((admin-path-p path)
       (route-admin request))
      ((public-path-p path)
       (route-public request))
      ;; Public static assets (favicons, manifest, robots.txt) bypass the
      ;; gate but go through the normal static-file route, not route-public.
      ((public-asset-path-p path)
       (route-request request))
      ((authenticated-request-p request)
       (route-request request))
      ;; Non-GET requests to gated paths: don't redirect, just 401. Redirects
      ;; are meaningful only to top-level browser navigations.
      ((not (eq (http-request-method request) :GET))
       (make-error-response 401))
      (t
       (authorize-redirect request)))))

#+shakespeare2/auth
(defun route-public (request)
  (let ((path (http-request-path request)))
    (cond
      ((string= path "/callback") (handle-callback request))
      ((string= path "/logout")   (handle-logout request))
      ((string= path "/healthz")  (healthz request))
      (t (make-error-response 404)))))

(defun route-request (request)
  "Serve the main application — WS upgrade, static assets, and /healthz."
  (let ((method (http-request-method request))
        (path   (http-request-path request)))
    (cond
      ((string= path "/healthz")
       (healthz request))
      ((and (eq method :GET) (string= path "/ws"))
       (if (origin-allowed-p request)
           :upgrade
           (make-error-response 403 "Origin not allowed.")))
      (t (or (serve-static request)
             (make-error-response 404))))))

;;; ---------------------------------------------------------------------------
;;; WebSocket protocol — stream poem tokens back to the browser.
;;;
;;; Frame conventions between server and client:
;;;   \x01 (SOH)  — begin generation (client clears the output box)
;;;   \x04 (EOT)  — end of generation
;;;   \x15 (NAK)  — followed by a short human-readable error message
;;; ---------------------------------------------------------------------------

;; Dynamic vars rather than defconstants: SBCL enforces strict EQL similarity
;; on defconstant reloads, and each FASL load produces a fresh string object.
(defvar *soh* (string #\Soh))
(defvar *eot* (string #\Eot))

(defun nak (message)
  (format nil "~c~a" #\Nak message))

(defun handle-ws-message (conn frame)
  "Receive a poem request, stream tokens back until Ollama is done or we hit
   one of the output caps (chars or lines), then send EOT. Errors are
   reported via NAK and logged server-side without leaking internal
   condition text."
  (unless (= (ws-frame-opcode frame) +ws-op-text+)
    (return-from handle-ws-message nil))
  (let* ((payload (ws-frame-payload frame))
         (text    (handler-case
                      (sb-ext:octets-to-string payload :external-format :utf-8)
                    (error () nil))))
    (cond
      ((or (null text) (zerop (length (string-trim '(#\Space #\Tab) text))))
       (ws-send conn (build-ws-text (nak "empty prompt"))))
      ((> (length text) *max-input-chars*)
       (ws-send conn (build-ws-text (nak "prompt too long"))))
      (t
       (log-info "poem request (~d chars)" (length text))
       (ws-send conn (build-ws-text *soh*))
       (let ((sent-chars 0)
             (sent-lines 0)
             (status :ok))
         (block stream
           (handler-case
               (stream-generate text
                 (lambda (token)
                   (let ((tchars (length token))
                         (tlines (count #\Newline token)))
                     (when (or (> (+ sent-chars tchars) *max-output-chars*)
                               (> (+ sent-lines tlines) *max-output-lines*))
                       (log-info "truncating output at ~d chars / ~d lines"
                                 sent-chars sent-lines)
                       (return-from stream))
                     (incf sent-chars tchars)
                     (incf sent-lines tlines)
                     (ws-send conn (build-ws-text token)))))
             (error (e)
               (log-error "ollama error: ~a" e)
               (setf status :failed))))
         (ecase status
           (:ok     (ws-send conn (build-ws-text *eot*)))
           (:failed (ws-send conn (build-ws-text (nak "generation failed")))))))))
  nil)

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun start ()
  (load-config)
  (load-static-files "static/"
                     :substitutions
                     `(("index.html"
                        ("__APP_TITLE__"   . ,*app-title*)
                        ("__APP_TAGLINE__" . ,*app-tagline*)
                        ;; __LOGOUT_LINK__ renders to the logout anchor in the
                        ;; auth build and to empty string in the public build.
                        ("__LOGOUT_LINK__" . ,#+shakespeare2/auth
                                              "<a href=\"/logout\" class=\"logout\">logout</a>"
                                              #-shakespeare2/auth ""))
                       ("favicon/site.webmanifest"
                        ("__APP_TITLE__"   . ,*app-title*))))
  ;; INIT-SESSIONS runs here (not at defvar time) because MAKE-STORE with
  ;; :reap-interval spawns a background thread, and SBCL's save-lisp-and-die
  ;; refuses to dump an image with live non-main threads. Deferring the
  ;; thread start until START() runtime keeps the build clean. Cleanup
  ;; registers itself with the framework's shutdown-hook machinery, so
  ;; no UNWIND-PROTECT wrap around START-SERVER is needed.
  #+shakespeare2/auth (init-sessions)
  (log-info "starting server")
  (start-server :host *server-host*
                :port *server-port*
                :handler #'handle-request
                :ws-handler #'handle-ws-message))

(defun main ()
  (start))
