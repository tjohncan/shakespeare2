(in-package :shakespeare2)

;;; ===========================================================================
;;; Confidential-client OAuth2 authorization-code + PKCE flow.
;;;
;;; Compiled in only when the SHAKESPEARE2_AUTH build flag is set.
;;;
;;; Design summary
;;; --------------
;;; * This server is a *confidential* OAuth2 client: the code -> token exchange
;;;   happens server-side with client_key_id + client_secret. The browser never
;;;   sees the access or refresh tokens.
;;; * A signed session cookie holds only an opaque session id. All token state
;;;   lives in an in-memory table keyed by that id.
;;; * PKCE verifier / OAuth2 state parameter live in the same session row,
;;;   generated on first contact (pre-auth phase) and cleared after /callback.
;;; * Token validity is tracked locally by expires-at. When the access token
;;;   expires we redirect back through /authorize — the auth server reissues
;;;   silently if the user still has a valid session there. No in-band
;;;   refresh_token grant is used (keeps handler logic synchronous).
;;; * Revocation before local expiry is not enforced on the hot path; the
;;;   INTROSPECT-TOKEN helper is provided for admin / background use.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Session store — web-skeleton's MAKE-STORE primitive, with TTL reaping.
;;;
;;; Two categories of session are reaped automatically:
;;;
;;;   * Expired: :expires-at exists and is in the past. Normal end-of-life
;;;     for an authenticated session once its access token has aged out.
;;;
;;;   * Stalled pre-auth: no :access-token and older than
;;;     *session-stale-seconds*. Covers users who opened /authorize and
;;;     never completed /callback, plus orphan rows from the ENSURE-SESSION
;;;     race (two concurrent first-visit requests both minting a new id).
;;;
;;; The framework owns the hash table + mutex + reaper thread and auto-
;;; registers cleanup via REGISTER-CLEANUP — the reaper stops when the
;;; server drains on SIGTERM / Ctrl-C. No app-side UNWIND-PROTECT wrap
;;; around START-SERVER is needed, and no sb-ext:*exit-hooks* dance.
;;;
;;; Canonical pattern: STATEFUL STORES ARE CREATED IN START(), NOT AT
;;; DEFVAR LOAD TIME. MAKE-STORE with :reap-interval spawns a background
;;; thread immediately, and SBCL's save-lisp-and-die refuses to dump an
;;; image with live non-main threads. Deferring init to START() keeps the
;;; build clean for both the REPL and the standalone binary.
;;; ---------------------------------------------------------------------------

(defvar *sessions* nil
  "Session store. Initialized in INIT-SESSIONS (called from START); see the
   section banner for why this is deferred rather than top-level.")

(defvar *session-reaper-interval* 60
  "Seconds between reaper sweeps.")

(defvar *session-stale-seconds* 600
  "Sessions without an access token are reaped once they are this old.")

(defun session-doomed-p (id sess)
  "Store expiry predicate — returns T for sessions that should be reaped.
   Called under the store's mutex during each sweep, so it must stay cheap
   (no I/O, no syscalls, no STORE-* calls on *SESSIONS* — that deadlocks)."
  (declare (ignore id))
  (let ((now     (get-universal-time))
        (exp     (getf sess :expires-at))
        (created (or (getf sess :created-at) 0)))
    (or (and exp (<= exp now))
        (and (not (getf sess :access-token))
             (> (- now created) *session-stale-seconds*)))))

(defun init-sessions ()
  "Create the session store with its reaper thread. Called from START()."
  (setf *sessions*
        (make-store :expiry-fn     #'session-doomed-p
                    :reap-interval *session-reaper-interval*)))

;;; ---------------------------------------------------------------------------
;;; PKCE (RFC 7636, S256)
;;; ---------------------------------------------------------------------------

(defun pkce-pair ()
  "Return (values VERIFIER CHALLENGE). Both are base64url strings.
   Verifier is 32 cryptographically random bytes, base64url-encoded —
   exactly the shape web-skeleton:RANDOM-TOKEN produces. Challenge is
   SHA-256(verifier-ASCII-bytes) base64url-encoded per RFC 7636 §4.2."
  (let* ((verifier (random-token))
         (digest   (sha256 (sb-ext:string-to-octets verifier
                                                    :external-format :ascii)))
         (challenge (base64url-encode digest)))
    (values verifier challenge)))

;;; ---------------------------------------------------------------------------
;;; URL encoding for application/x-www-form-urlencoded bodies and query strings
;;; ---------------------------------------------------------------------------

(defun url-encode (s)
  "Percent-encode S per application/x-www-form-urlencoded.
   Unreserved set: ALPHA / DIGIT / '-' / '.' / '_' / '~' (RFC 3986)."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets s :external-format :utf-8)
          do (cond
               ((or (<= 65 byte 90)                         ; A-Z
                    (<= 97 byte 122)                        ; a-z
                    (<= 48 byte 57)                         ; 0-9
                    (= byte 45) (= byte 46)                 ; - .
                    (= byte 95) (= byte 126))               ; _ ~
                (write-char (code-char byte) out))
               (t
                (format out "%~2,'0X" byte))))))

(defun build-form-body (pairs)
  "Serialize ((KEY . VALUE) ...) as an application/x-www-form-urlencoded body.
   Keys with NIL values are dropped. Values are coerced to strings."
  (with-output-to-string (out)
    (loop for first = t then nil
          for (k . v) in pairs
          when v do
            (unless first (write-char #\& out))
            (write-string (url-encode k) out)
            (write-char #\= out)
            (write-string (url-encode (if (stringp v) v (princ-to-string v)))
                          out))))

;;; ---------------------------------------------------------------------------
;;; Session cookie helpers — thin wrappers over web-skeleton's BUILD-COOKIE
;;; / DELETE-COOKIE. The framework validates attribute syntax, enforces
;;; Secure when SameSite=None, and rejects CR/LF/semicolon in name or
;;; value, so hand-rolling the Set-Cookie string is strictly worse.
;;; ---------------------------------------------------------------------------

(defun set-session-cookie (response id)
  "Attach a Set-Cookie for the session id. HttpOnly + SameSite=Lax are the
   framework defaults; Secure tracks *SESSION-COOKIE-SECURE* so local
   HTTP testing still accepts the cookie. ADD- (not SET-) so a future caller
   that wants to emit a second cookie on the same response can — set- would
   silently replace the previous Set-Cookie line."
  (add-response-header response "set-cookie"
                       (build-cookie *session-cookie-name* id
                                     :secure  *session-cookie-secure*
                                     :max-age *session-ttl-seconds*))
  response)

(defun clear-session-cookie (response)
  "Attach a Set-Cookie that expires the session cookie on the browser."
  (add-response-header response "set-cookie"
                       (delete-cookie *session-cookie-name*))
  response)

(defun ensure-session (request)
  "Return (values SESSION-ID SESSION-PLIST NEW-P).
   Creates and stores an empty session if the request has no cookie.
   Caller is responsible for setting the Set-Cookie header when NEW-P is T."
  (let* ((cookie (get-cookie request *session-cookie-name*))
         (sess   (and cookie (store-get *sessions* cookie))))
    (if sess
        (values cookie sess nil)
        (let ((id (random-token))
              (plist (list :created-at (get-universal-time))))
          (store-set *sessions* id plist)
          (values id plist t)))))

;;; ---------------------------------------------------------------------------
;;; Authorize redirect
;;; ---------------------------------------------------------------------------

(defun build-authorize-url (challenge state)
  (format nil "~a~a?~a"
          *auth-server-url* *auth-authorize-path*
          (build-form-body
           `(("response_type"         . "code")
             ("client_id"             . ,*auth-client-id*)
             ("redirect_uri"          . ,*auth-redirect-uri*)
             ("code_challenge"        . ,challenge)
             ("code_challenge_method" . "S256")
             ("state"                 . ,state)
             ,@(when (and *auth-scope* (> (length *auth-scope*) 0))
                 `(("scope" . ,*auth-scope*)))))))

(defun make-redirect (location &key set-cookie-id clear-cookie)
  (let ((resp (make-text-response 302 "")))
    (set-response-header resp "location" location)
    (set-response-header resp "cache-control" "no-store")
    (cond
      (clear-cookie  (clear-session-cookie resp))
      (set-cookie-id (set-session-cookie resp set-cookie-id)))
    resp))

(defun authorize-redirect (request)
  "Stamp the session with a fresh PKCE pair and state, then 302 to /authorize."
  (multiple-value-bind (id sess new-p) (ensure-session request)
    (declare (ignore sess))
    (multiple-value-bind (verifier challenge) (pkce-pair)
      (let ((state (random-token :bytes 16)))
        (store-update-plist *sessions* id
                            :pkce-verifier verifier
                            :oauth-state   state
                            :created-at    (get-universal-time))
        (make-redirect (build-authorize-url challenge state)
                       :set-cookie-id (when new-p id))))))

;;; ---------------------------------------------------------------------------
;;; /callback — exchange code for tokens (async defer-to-fetch)
;;; ---------------------------------------------------------------------------

(defun parse-query (request)
  "Parse the request query string into an alist. Returns NIL if empty."
  (let ((q (http-request-query request)))
    (when (and q (> (length q) 0))
      (parse-query-string q))))

(defun qparam (alist name)
  (cdr (assoc name alist :test #'string=)))

(defun handle-callback (request)
  "OAuth2 redirect target. Validates state and PKCE session, exchanges the
   authorization code for tokens at /token, stores them in the session,
   redirects to /."
  (let* ((id   (get-cookie request *session-cookie-name*))
         (sess (and id (store-get *sessions* id)))
         (params (parse-query request))
         (code   (qparam params "code"))
         (state  (qparam params "state"))
         (err    (qparam params "error")))
    (cond
      (err
       (log-warn "auth callback error from server: ~a (~a)"
                 err (qparam params "error_description"))
       (make-error-response 400 "Authorization failed."))
      ((not (and sess code state))
       (log-warn "auth callback missing session/code/state (cookie=~a)"
                 (if id "present" "absent"))
       (make-error-response 400 "Invalid callback."))
      ((not (and (getf sess :oauth-state)
                 (string= state (getf sess :oauth-state))))
       (log-warn "auth callback state mismatch for session ~a"
                 (if id (subseq id 0 8) "?"))
       (make-error-response 400 "State mismatch."))
      (t
       (exchange-code-for-tokens id code (getf sess :pkce-verifier))))))

(defun exchange-code-for-tokens (session-id code verifier)
  "Return a fetch continuation that performs the /token POST and, on success,
   stores the tokens in the session and redirects to /. DEFER-TO-FETCH is
   the readability form of HTTP-FETCH: the handler's return value IS the
   async control-flow signal, and the name makes the intent explicit at
   the call site."
  (let ((url  (concatenate 'string *auth-server-url* *auth-token-path*))
        (body (build-form-body
               `(("grant_type"    . "authorization_code")
                 ("code"          . ,code)
                 ("redirect_uri"  . ,*auth-redirect-uri*)
                 ("client_id"     . ,*auth-client-id*)
                 ("client_key_id" . ,*auth-client-key-id*)
                 ("client_secret" . ,*auth-client-secret*)
                 ("code_verifier" . ,verifier)))))
    (defer-to-fetch :post url
      :headers '(("content-type" . "application/x-www-form-urlencoded"))
      :body body
      :then (lambda (status headers body-bytes)
              (declare (ignore headers))
              (on-token-response session-id status body-bytes)))))

(defun on-token-response (session-id status body-bytes)
  "Fetch-callback body for the /token exchange. STATUS is NIL on the
   cleanup-sentinel path (upstream TCP / TLS / DNS failure, inbound
   closed mid-fetch, drain) per DEPLOYMENT.md — exchange-code-for-tokens'
   inbound is already gone at that point, so the return value is
   discarded and we just drop the in-flight pre-auth session so the
   reaper doesn't have to."
  (cond
    ((null status)
     (log-warn "auth: /token fetch aborted (inbound closed or upstream failure)")
     (store-delete *sessions* session-id)
     nil)
    ((/= status 200)
     (log-error "auth: /token returned ~d: ~a" status (octets-to-utf8 body-bytes))
     (make-error-response 502 "Authentication failed."))
    (t
     (let* ((json    (handler-case (json-parse (octets-to-utf8 body-bytes))
                       (error () nil)))
            (access  (and json (json-get json "access_token")))
            (refresh (and json (json-get json "refresh_token")))
            (expires (and json (json-get json "expires_in"))))
       (cond
         ((not (and (stringp access) (integerp expires) (> expires 0)))
          (log-error "auth: /token response missing/invalid fields")
          (make-error-response 502 "Authentication failed."))
         (t
          ;; Rotate the session id at the privilege boundary. Mint a fresh
          ;; id, store the authenticated plist under it, drop the old row.
          ;; Defeats fixation attacks where a planted pre-auth cookie would
          ;; otherwise carry over into the authenticated session, and is
          ;; the OAuth2/OIDC best-practice response to a successful /token
          ;; exchange. The new plist starts clean — no :pkce-verifier or
          ;; :oauth-state carried forward — and :created-at resets so the
          ;; reaper's age math reflects the post-auth lifetime.
          (let ((new-id (random-token)))
            (store-set *sessions* new-id
                       (list :access-token  access
                             :refresh-token refresh
                             :expires-at    (+ (get-universal-time) expires)
                             :created-at    (get-universal-time)))
            (store-delete *sessions* session-id)
            (log-info "auth: session ~a authenticated (rotated to ~a, exp ~ds)"
                      (subseq session-id 0 8) (subseq new-id 0 8) expires)
            (make-redirect "/" :set-cookie-id new-id))))))))

(defun octets-to-utf8 (bytes)
  (if (null bytes)
      ""
      (handler-case
          (sb-ext:octets-to-string bytes :external-format :utf-8)
        (error () ""))))

;;; ---------------------------------------------------------------------------
;;; /logout — revoke token (best-effort), clear session, redirect home.
;;; ---------------------------------------------------------------------------

(defun handle-logout (request)
  (let* ((id   (get-cookie request *session-cookie-name*))
         (sess (and id (store-get *sessions* id)))
         (access (getf sess :access-token))
         (destination (or *auth-post-logout-redirect* "/")))
    (when id (store-delete *sessions* id))
    (if access
        (let ((url  (concatenate 'string *auth-server-url* *auth-revoke-path*))
              (body (build-form-body
                     `(("token"           . ,access)
                       ("token_type_hint" . "access_token")
                       ("client_id"       . ,*auth-client-id*)
                       ("client_key_id"   . ,*auth-client-key-id*)
                       ("client_secret"   . ,*auth-client-secret*)))))
          (defer-to-fetch :post url
            :headers '(("content-type" . "application/x-www-form-urlencoded"))
            :body body
            :then (lambda (status headers body-bytes)
                    (declare (ignore headers body-bytes))
                    ;; STATUS is NIL on the cleanup-sentinel path; /revoke
                    ;; is best-effort anyway, so treat NIL the same as a
                    ;; non-200 — the local session is already deleted
                    ;; above and we just redirect home.
                    (cond
                      ((null status)
                       (log-warn "auth: /revoke fetch aborted"))
                      ((/= status 200)
                       (log-warn "auth: /revoke returned ~d" status)))
                    (make-redirect destination :clear-cookie t))))
        (make-redirect destination :clear-cookie t))))

;;; ---------------------------------------------------------------------------
;;; Gate check — used from handle-request on every non-public path
;;; ---------------------------------------------------------------------------

(defun session-live-p (sess)
  (and (getf sess :access-token)
       (> (or (getf sess :expires-at) 0) (get-universal-time))))

(defun public-path-p (path)
  "Paths with public handlers (route-public)."
  (or (string= path "/callback")
      (string= path "/logout")
      (string= path "/login")
      (string= path "/healthz")))

(defun public-asset-path-p (path)
  "Static assets that must be reachable without a session: icons, the PWA
   manifest, robots.txt, plus the CSS and the preview-shell JS so the
   public landing page renders correctly for un-cookied visitors. Loaded
   by <link>/<script> tags without credentials, so gating them causes a
   cross-origin redirect to /authorize that breaks browser
   manifest/favicon handling and produces a flash of unstyled preview."
  (or (string= path "/favicon.ico")
      (string= path "/robots.txt")
      (string= path "/style.css")
      (string= path "/preview.js")
      (and (>= (length path) 9)
           (string= path "/favicon/" :end1 9))))

(defun authenticated-request-p (request)
  "Return T when REQUEST carries a cookie tied to a live session."
  (let ((id (get-cookie request *session-cookie-name*)))
    (and id (session-live-p (store-get *sessions* id)))))

;;; ---------------------------------------------------------------------------
;;; /introspect builder — RFC 7662 resource-server validation of a token.
;;;
;;; Returned as a DEFER-TO-FETCH continuation so the caller can chain it
;;; from a handler (e.g. a future admin endpoint that verifies a token
;;; before acting on it). The THEN callback receives the parsed
;;; introspection result as a JSON alist (or NIL on failure) plus the
;;; status code.
;;; ---------------------------------------------------------------------------

(defun introspect-token (token then)
  "Return a fetch continuation that POSTs /introspect with the
   resource-server creds. THEN is called with (active-p json-or-nil
   status) after the response. Requires AUTH_RS_* env vars to be set."
  (unless (and *auth-rs-id* *auth-rs-key-id* *auth-rs-secret*)
    (error "introspect-token requires AUTH_RS_ID/KEY_ID/SECRET to be set"))
  (let ((url  (concatenate 'string *auth-server-url* *auth-introspect-path*))
        (body (build-form-body
               `(("token"                  . ,token)
                 ("token_type_hint"        . "access_token")
                 ("resource_server_id"     . ,*auth-rs-id*)
                 ("resource_server_key_id" . ,*auth-rs-key-id*)
                 ("resource_server_secret" . ,*auth-rs-secret*)))))
    (defer-to-fetch :post url
      :headers '(("content-type" . "application/x-www-form-urlencoded"))
      :body body
      :then (lambda (status headers body-bytes)
              (declare (ignore headers))
              ;; STATUS is NIL on the cleanup-sentinel path. Hand the
              ;; caller's THEN the shape it expects (active=NIL, json=NIL,
              ;; status=NIL) so callers don't have to re-check for a
              ;; separate abort signal — the DEPLOYMENT.md contract lives
              ;; at one layer.
              (let* ((json (when (eql status 200)
                             (handler-case
                                 (json-parse (octets-to-utf8 body-bytes))
                               (error () nil))))
                     (active (and json (eq (json-get json "active") t))))
                (funcall then active json status))))))
