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
      ;; Top-level GET to / (or /index.html) from an unauthenticated visitor:
      ;; serve the public preview shell instead of bouncing to /authorize.
      ;; Spares casual visitors the auth-server runaround. Other gated GETs
      ;; still redirect — preserves deep-linkable bookmarks landing back
      ;; where the user expected after auth.
      ((preview-path-p path)
       (serve-preview request))
      (t
       (authorize-redirect request)))))

#+shakespeare2/auth
(defun route-public (request)
  (let ((path (http-request-path request)))
    (cond
      ((string= path "/callback") (handle-callback request))
      ((string= path "/logout")   (handle-logout request))
      ((string= path "/login")    (handle-login request))
      ((string= path "/healthz")  (healthz request))
      (t (make-error-response 404)))))

#+shakespeare2/auth
(defun preview-path-p (path)
  "Paths the preview shell answers for unauthenticated GETs."
  (or (string= path "/")
      (string= path "/index.html")))

#+shakespeare2/auth
(defun serve-preview (request)
  "Serve the preview shell out of the static cache.
   The framework's SERVE-STATIC keys on (HTTP-REQUEST-PATH REQUEST), and the
   static cache is its private state, so the smallest way to redirect / to
   /preview.html without forking the cache is to swap the path field for the
   duration of the lookup. UNWIND-PROTECT restores the original path before
   we return so the framework's post-handler log lines (which read
   HTTP-REQUEST-PATH after we hand the response back) record what the
   visitor actually requested."
  (let ((original-path (http-request-path request)))
    (unwind-protect
         (progn
           (setf (http-request-path request) "/preview.html")
           (or (serve-static request)
               (make-error-response 404)))
      (setf (http-request-path request) original-path))))

#+shakespeare2/auth
(defun handle-login (request)
  "Public entry point for the auth flow — what the preview's 'log in' button
   targets. Authed visitors short-circuit to / so a stray click doesn't burn
   a fresh PKCE round-trip."
  (if (authenticated-request-p request)
      (make-redirect "/")
      (authorize-redirect request)))

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
  "Receive a poem request and start a generation streaming back to CONN.

   Returns as soon as the request is away — it does not wait for the poem.
   Tokens are written from ON-TOKEN and the terminator from ON-DONE, both
   called later from the event loop, so this function returning means only
   that the generation began. Errors are reported via NAK and logged
   server-side without leaking internal condition text.

   The output caps stop this connection being *sent* more than its share, and
   stop the upstream generating it too — the cap returns :STOP and the fetch
   ends. See the note at the cap."
  (unless (= (ws-frame-opcode frame) +ws-op-text+)
    (return-from handle-ws-message nil))
  (let* ((payload (ws-frame-payload frame))
         (text    (handler-case
                      (sb-ext:octets-to-string payload :external-format :utf-8)
                    (error () nil))))
    (cond
      ((or (null text)
           (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
       (ws-send conn (build-ws-text (nak "empty prompt"))))
      ((> (length text) *max-input-chars*)
       (ws-send conn (build-ws-text (nak "prompt too long"))))
      (t
       (log-info "poem request (~d chars)" (length text))
       (ws-send conn (build-ws-text *soh*))
       (let ((sent-chars 0)
             (sent-lines 0)
             (truncated nil))
         ;; The terminator moved into ON-DONE, and it had to. START-GENERATION
         ;; returns as soon as the request is away now, so anything after this
         ;; form runs before a single token has arrived — an EOT sent here
         ;; would close the output box ahead of the poem.
         ;;
         ;; TRUNCATED replaces the BLOCK / RETURN-FROM the cap used to use.
         ;; That exit unwound out of the producer, and for :OLLAMA out of the
         ;; HTTP read, whose UNWIND-PROTECT closed the socket and stopped the
         ;; model. There is no stack to unwind now: ON-TOKEN is called from
         ;; the event loop, and this frame returned long ago.
         ;;
         ;; So the cap returns :STOP instead of unwinding, and a verdict
         ;; travels where an exit could not. The upstream is closed, this
         ;; connection is untouched, and the client sees what it always saw:
         ;; the same truncated poem, the same EOT. What changed is on the
         ;; other side — the model stops generating tokens nobody will read.
         ;;
         ;; TRUNCATED still exists and still guards the cap, because :STOP
         ;; ends the fetch and not the pass: tokens already decoded from
         ;; bytes in hand keep arriving after the verdict is given, and
         ;; without the guard they would be sent past the cap that just
         ;; refused them.
         (start-generation conn text
           :on-token
           (lambda (token)
             (if truncated
                 ;; Say it again for every token that arrives after the
                 ;; verdict. Saying it once would be enough for the framework,
                 ;; which makes the stop sticky within a pass, but relying on
                 ;; that would put this app's correctness inside a framework
                 ;; detail it does not own.
                 :stop
                 (let ((tchars (length token))
                       (tlines (count #\Newline token)))
                   (cond
                     ((or (> (+ sent-chars tchars) *max-output-chars*)
                          (> (+ sent-lines tlines) *max-output-lines*))
                      (setf truncated t)
                      (log-info "truncating output at ~d chars / ~d lines ~
                                 (stopping the upstream)"
                                sent-chars sent-lines)
                      :stop)
                     (t
                      (incf sent-chars tchars)
                      (incf sent-lines tlines)
                      (ws-send conn (build-ws-text token))
                      nil)))))
           :on-done
           (lambda (result)
             ;; A truncated poem is a normal ending in this app and keeps its
             ;; EOT rather than becoming a NAK. That was true before anything
             ;; could stop the upstream and it is true now, but it stopped
             ;; being free: the fetch a cap stops ends through the framework's
             ;; abort sentinel, which is the same NIL status a failed upstream
             ;; delivers. Reported as :FAILED it would turn every truncated
             ;; poem into "generation failed" — a working feature reporting
             ;; itself broken.
             ;;
             ;; The producer answers :STOPPED instead, because the producer is
             ;; what knows. An earlier note here predicted TRUNCATED would
             ;; become load-bearing at this callback; it did not, and the
             ;; reason is worth keeping. Consulting TRUNCATED would have this
             ;; frame re-derive a fact the seam already had, and two readers
             ;; of one fact is the shape both repos refuse.
             (ecase result
               (:ok      (ws-send conn (build-ws-text *eot*)))
               (:stopped (ws-send conn (build-ws-text *eot*)))
               (:failed  (ws-send conn (build-ws-text
                                        (nak "generation failed")))))))))))
  nil)

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun escape-html-attr (s)
  "Minimal HTML-attribute escape — operator-controlled BACKLINK_URL still
   passes through APPLY-SUBSTITUTIONS into a quoted attribute, so a stray
   '\"' would terminate the attribute. Keep the substitution byte-exact and
   defend at the build site."
  (with-output-to-string (out)
    (loop for c across s do
      (case c
        (#\& (write-string "&amp;"  out))
        (#\< (write-string "&lt;"   out))
        (#\> (write-string "&gt;"   out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&#39;"  out))
        (t   (write-char c out))))))

(defun static-substitutions ()
  "Build the :substitutions arg for LOAD-STATIC-FILES. Inlined into START at
   one point — pulled out so the auth-build conditionals don't bury the
   start-up flow they sit inside, and so the preview-shell entries can sit
   next to their gated counterpart cleanly."
  (let* ((logout-link
          #+shakespeare2/auth "<a href=\"/logout\" class=\"logout\">logout</a>"
          #-shakespeare2/auth "")
         (backlink
          (if *backlink-url*
              (format nil "<a href=\"~a\" class=\"backlink\">exit</a>"
                      (escape-html-attr *backlink-url*))
              "")))
    `(("index.html"
       ("__APP_TITLE__"   . ,*app-title*)
       ("__APP_TAGLINE__" . ,*app-tagline*)
       ;; __LOGOUT_LINK__ renders to the logout anchor in the auth build
       ;; and to empty string in the public build. __BACKLINK__ is the same
       ;; anchor used by the preview shell — visible on the authed page when
       ;; BACKLINK_URL is set, empty when it isn't.
       ("__LOGOUT_LINK__" . ,logout-link)
       ("__BACKLINK__"    . ,backlink))
      ("favicon/site.webmanifest"
       ("__APP_TITLE__"   . ,*app-title*))
      #+shakespeare2/auth
      ("preview.html"
       ("__APP_TITLE__"   . ,*app-title*)
       ("__APP_TAGLINE__" . ,*app-tagline*)
       ("__BACKLINK__"    . ,backlink)))))

(defun static-cache-control (url-path)
  "Cache-Control resolver for LOAD-STATIC-FILES. /index.html and
   /preview.html vary by auth state — both render under the URL '/' (the
   preview shell via SERVE-PREVIEW's path swap, the authed app via the
   directory-index fallback) — so the framework's default 'public,
   max-age=3600' poisons the browser's '/' cache entry: an unauthed visit
   stamps the preview HTML there for an hour, and the post-/callback
   redirect to '/' is served from cache without ever asking the server, so
   the freshly-set session cookie is irrelevant. Same trap in reverse for
   logout. NO-STORE forces every '/' load to hit the server, which is
   cheap on a low-traffic site and the only reliable way to keep the
   cached content aligned with the cookie. Other assets (CSS, JS,
   favicons) are auth-state-invariant, so they keep the long TTL."
  (if (or (string= url-path "/index.html")
          (string= url-path "/preview.html"))
      "no-store"
      "public, max-age=3600"))

(defun start ()
  (load-config)
  (load-static-files "static/"
                     :cache-control #'static-cache-control
                     :substitutions (static-substitutions))
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
