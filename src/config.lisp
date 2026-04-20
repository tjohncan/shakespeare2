(in-package :shakespeare2)

;;; ===========================================================================
;;; Runtime configuration
;;;
;;; All settings are read from environment variables at server startup.
;;; Defaults target the local Docker Compose setup.
;;; ===========================================================================

(defvar *server-host*    nil "HTTP bind address as a 4-element octet vector.")
(defvar *server-port*    nil "HTTP listen port.")
(defvar *ollama-host*    nil "Ollama hostname.")
(defvar *ollama-port*    nil "Ollama port.")
(defvar *ollama-model*       nil "Model name passed to Ollama.")
(defvar *ollama-temperature* nil "Sampling temperature (nil = Ollama default).")
(defvar *ollama-num-predict* nil "Max tokens to generate (nil = Ollama default).")
(defvar *system-prompt*  nil "System prompt loaded from SPIRIT.md.")
(defvar *app-title*      nil "App title — shown in <title> and <h1>.")
(defvar *app-tagline*    nil "Tagline — shown under the title in the UI.")

(defvar *allowed-origins* nil
  "List of Origin URLs permitted to open WebSocket connections.
   Stored lowercased; comparison is case-insensitive per RFC 3986 host rules.
   Empty list = allow only same-origin (no Origin header, or Origin matches Host).")

(defvar *max-input-chars* 200
  "Hard cap on characters accepted in a poem prompt.
   A ridiculousness firewall — the UX is short inspiration text.")

(defvar *max-output-chars* 3000
  "Hard cap on characters streamed to the client per generation.
   Defence against a runaway model — SPIRIT asks for ≤24 lines, but we don't
   trust it to behave.")

(defvar *max-output-lines* 40
  "Hard cap on newlines emitted per generation.
   Complements *max-output-chars*: catches the sparse-output pathology
   (many newlines, few total chars) that a character cap alone misses.")

#+shakespeare2/auth
(progn
  ;; Endpoint locations — defaulted for standard OAuth2/RFC paths, override to fit.
  (defvar *auth-server-url*     nil "Base URL of the auth server (https://...).")
  (defvar *auth-authorize-path* nil "Path of the authorization endpoint.")
  (defvar *auth-token-path*     nil "Path of the token endpoint.")
  (defvar *auth-introspect-path* nil "Path of the RFC 7662 introspection endpoint.")
  (defvar *auth-revoke-path*    nil "Path of the RFC 7009 revocation endpoint.")
  ;; Redirect URI must be registered with the auth-server for this client.
  (defvar *auth-redirect-uri*   nil "Absolute URL of our /callback endpoint.")
  (defvar *auth-post-logout-redirect* nil "URL to send users to after /logout.")
  ;; Scope requested at /authorize (space-separated, e.g. \"openid profile\").
  (defvar *auth-scope*          nil "OAuth2 scope string, or NIL to omit.")
  ;; Confidential-client credentials (client_key_id + client_secret).
  (defvar *auth-client-id*        nil "OAuth2 client UUID.")
  (defvar *auth-client-key-id*    nil "Client key UUID — identifies which secret.")
  (defvar *auth-client-secret*    nil "Client secret (plaintext).")
  ;; Resource-server credentials — used for /introspect (RS role).
  (defvar *auth-rs-id*            nil "Resource server UUID.")
  (defvar *auth-rs-key-id*        nil "Resource server key UUID.")
  (defvar *auth-rs-secret*        nil "Resource server secret.")
  ;; Session policy.
  (defvar *session-cookie-name*   nil "Name of the session cookie we set.")
  (defvar *session-cookie-secure* t   "Set Secure flag on the session cookie.")
  (defvar *session-ttl-seconds*   nil "Max session lifetime in seconds.")
  ;; Localhost-only admin API. Endpoints stay dark (404) unless this is set
  ;; AND the resource-server credentials above are all present.
  (defvar *admin-api-token*       nil "Shared secret for X-Admin-Token."))

(defun getenv-or (name default)
  "Like UIOP:GETENV but treats an empty value as unset. Matches docker-compose
   semantics: ${VAR-} expands to \"\" when VAR is unset."
  (let ((v (uiop:getenv name)))
    (if (and v (> (length v) 0)) v default)))

(defun getenv-bool (name default)
  "Parse a boolean env var. True = \"1\", \"t\", \"true\", \"yes\" (any case).
   Empty / unset falls through to DEFAULT."
  (let ((v (getenv-or name nil)))
    (if v
        (let ((lc (string-downcase v)))
          (or (string= lc "1") (string= lc "t")
              (string= lc "true") (string= lc "yes")))
        default)))

(defun parse-pos-int-env (name)
  "Parse a strictly-positive integer from env var NAME. Returns the integer
   or NIL (if unset, empty, malformed, or ≤ 0). Use for limit-style knobs
   where 0 would disable the guard."
  (let ((v (uiop:getenv name)))
    (when (and v (> (length v) 0))
      (let ((n (handler-case (parse-integer v) (error () nil))))
        (when (and n (> n 0)) n)))))

(defun parse-nonneg-float (s)
  "Parse S as a non-negative decimal number. Returns a DOUBLE-FLOAT or NIL.
   Accepts digits with at most one dot; rejects signs, exponents, and
   whitespace. Does not call READ, so it's safe for untrusted input."
  (when (and (stringp s) (> (length s) 0))
    (let ((num 0d0) (denom 1d0) (saw-dot nil) (has-digits nil))
      (loop for c across s do
        (let ((d (digit-char-p c)))
          (cond
            (d
             (setf has-digits t
                   num (+ (* num 10d0) d))
             (when saw-dot (setf denom (* denom 10d0))))
            ((and (char= c #\.) (not saw-dot))
             (setf saw-dot t))
            (t (return-from parse-nonneg-float nil)))))
      (when has-digits (/ num denom)))))

(defun load-soul (path)
  "Load the system prompt from PATH. Falls back to a minimal prompt if missing."
  (with-open-file (s path :if-does-not-exist nil)
    (if s
        (let ((buf (make-string (file-length s))))
          (read-sequence buf s)
          (string-right-trim '(#\Newline #\Return #\Space #\Tab) buf))
        (progn
          (log-warn "SPIRIT.md not found at ~a — using bare fallback" path)
          "You are a poet. Respond only with the poem."))))

(defun parse-ipv4 (s)
  "Parse a dotted-decimal IPv4 string into a 4-element octet vector.
   Signals an error on malformed input rather than returning garbage."
  (let ((parts (loop with start = 0
                     for i from 0 to (length s)
                     when (or (= i (length s)) (char= (char s i) #\.))
                       collect (parse-integer s :start start :end i)
                       and do (setf start (1+ i)))))
    (unless (= (length parts) 4)
      (error "HOST must be a dotted-quad IPv4 address, got: ~s" s))
    (dolist (p parts)
      (unless (<= 0 p 255)
        (error "HOST octet out of range in ~s" s)))
    (coerce parts 'vector)))

(defun split-csv (s)
  "Split a comma-separated string, trim whitespace, drop empties."
  (when (and s (> (length s) 0))
    (let ((out '())
          (start 0))
      (dotimes (i (1+ (length s)))
        (when (or (= i (length s)) (char= (char s i) #\,))
          (let ((piece (string-trim '(#\Space #\Tab)
                                    (subseq s start i))))
            (when (> (length piece) 0)
              (push piece out)))
          (setf start (1+ i))))
      (nreverse out))))

(defun require-env (name)
  "Fetch an env var that must be non-empty. Error with a clear message otherwise."
  (let ((v (uiop:getenv name)))
    (if (and v (> (length v) 0))
        v
        (error "~a is required but not set (or empty)" name))))

(defun load-config ()
  "Read all runtime configuration from environment variables."
  ;; Log level first so subsequent debug lines during config + static-file
  ;; load fire at the chosen verbosity. Unknown values fall through to :debug.
  (let ((lvl (string-downcase (getenv-or "LOG_LEVEL" "debug"))))
    (setf *log-level*
          (cond ((string= lvl "debug") :debug)
                ((string= lvl "info")  :info)
                ((string= lvl "warn")  :warn)
                ((string= lvl "error") :error)
                (t :debug))))
  (setf *server-host*   (parse-ipv4 (getenv-or "HOST" "127.0.0.1")))
  (setf *server-port*   (parse-integer (getenv-or "PORT" "8080")))
  (setf *ollama-host*   (getenv-or "OLLAMA_HOST" "ollama"))
  (setf *ollama-port*   (parse-integer (getenv-or "OLLAMA_PORT" "11434")))
  (setf *ollama-model*  (getenv-or "OLLAMA_MODEL" "dolphin-llama3:8b"))
  (setf *ollama-temperature* (parse-nonneg-float (uiop:getenv "OLLAMA_TEMPERATURE")))
  (let ((pred (uiop:getenv "OLLAMA_NUM_PREDICT")))
    (setf *ollama-num-predict*
          (when (and pred (> (length pred) 0))
            (handler-case (parse-integer pred) (error () nil)))))
  (setf *system-prompt* (load-soul (getenv-or "SPIRIT_PATH" "SPIRIT.md")))
  (setf *app-title*   (getenv-or "APP_TITLE"   "shakespeare2"))
  (setf *app-tagline* (getenv-or "APP_TAGLINE" "request a poem in the style of the bard"))
  (setf *allowed-origins*
        (mapcar #'string-downcase (split-csv (uiop:getenv "ALLOWED_ORIGINS"))))
  (setf *max-input-chars*  (or (parse-pos-int-env "MAX_INPUT_CHARS")  *max-input-chars*))
  (setf *max-output-chars* (or (parse-pos-int-env "MAX_OUTPUT_CHARS") *max-output-chars*))
  (setf *max-output-lines* (or (parse-pos-int-env "MAX_OUTPUT_LINES") *max-output-lines*))
  #+shakespeare2/auth
  (load-auth-config)
  (log-info "config: host=~{~d~^.~} port=~d ollama=~a:~d model=~a auth=~a origins=~a"
            (coerce *server-host* 'list) *server-port*
            *ollama-host* *ollama-port* *ollama-model*
            #+shakespeare2/auth "on" #-shakespeare2/auth "off"
            (or *allowed-origins* '(:same-origin)))
  (log-info "config: caps in=~d out=~d chars / ~d lines"
            *max-input-chars* *max-output-chars* *max-output-lines*))

#+shakespeare2/auth
(defun load-auth-config ()
  "Read auth-specific configuration. All required fields must be present."
  (setf *auth-server-url*      (require-env "AUTH_SERVER_URL"))
  (setf *auth-redirect-uri*    (require-env "AUTH_REDIRECT_URI"))
  (setf *auth-client-id*       (require-env "AUTH_CLIENT_ID"))
  (setf *auth-client-key-id*   (require-env "AUTH_CLIENT_KEY_ID"))
  (setf *auth-client-secret*   (require-env "AUTH_CLIENT_SECRET"))
  ;; Resource-server credentials are optional — only needed if we ever call /introspect.
  (setf *auth-rs-id*           (uiop:getenv "AUTH_RS_ID"))
  (setf *auth-rs-key-id*       (uiop:getenv "AUTH_RS_KEY_ID"))
  (setf *auth-rs-secret*       (uiop:getenv "AUTH_RS_SECRET"))
  ;; Endpoint paths (auth-server).
  (setf *auth-authorize-path*  (getenv-or "AUTH_AUTHORIZE_PATH"  "/authorize"))
  (setf *auth-token-path*      (getenv-or "AUTH_TOKEN_PATH"      "/token"))
  (setf *auth-introspect-path* (getenv-or "AUTH_INTROSPECT_PATH" "/introspect"))
  (setf *auth-revoke-path*     (getenv-or "AUTH_REVOKE_PATH"     "/revoke"))
  (setf *auth-post-logout-redirect* (uiop:getenv "AUTH_POST_LOGOUT_REDIRECT"))
  (setf *auth-scope*           (uiop:getenv "AUTH_SCOPE"))
  (setf *session-cookie-name*  (getenv-or "SESSION_COOKIE_NAME" "shakespeare2_session"))
  (setf *session-cookie-secure* (getenv-bool "SESSION_COOKIE_SECURE" t))
  (setf *session-ttl-seconds*
        (parse-integer (getenv-or "SESSION_TTL_SECONDS" "28800"))) ; 8h default
  ;; Admin API (optional) — only useful alongside AUTH_RS_* credentials.
  (setf *admin-api-token*      (uiop:getenv "ADMIN_API_TOKEN")))
