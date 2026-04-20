(in-package :shakespeare2)

;;; ===========================================================================
;;; Localhost-only admin endpoints — wrappers around the auth-server's RS API.
;;;
;;; Exposed endpoints (JSON in, JSON out):
;;;
;;;   POST   /admin/users                find-or-create (invite) a user
;;;   POST   /admin/users/lookup         look up a user (no create)
;;;   POST   /admin/client-users         link user to our client
;;;   DELETE /admin/client-users         unlink user from our client
;;;   POST   /admin/client-users/list    list users linked to our client
;;;
;;; Security
;;; --------
;;; * Compiled only when SHAKESPEARE2_AUTH is set.
;;; * Active only when AUTH_RS_{ID,KEY_ID,SECRET} and ADMIN_API_TOKEN are all
;;;   configured. Otherwise every /admin/* path returns 404.
;;; * Every request must carry an X-Admin-Token header matching
;;;   *admin-api-token* (constant-time compared).
;;; * nginx rejects /admin/* for external traffic as defence-in-depth; the
;;;   intended operator path is `docker exec app curl 127.0.0.1:8080/admin/...`.
;;;
;;; Each handler is either a direct JSON response (validation failures) or a
;;; DEFER-TO-FETCH continuation that relays the call to the auth-server with
;;; our RS credentials injected. The :then callback forwards the upstream
;;; response back to the admin caller unchanged (status + JSON body).
;;; ===========================================================================

(defun admin-enabled-p ()
  (and *auth-rs-id* *auth-rs-key-id* *auth-rs-secret* *admin-api-token*))

(defun admin-auth-check (request)
  "Constant-time comparison of the X-Admin-Token header to *admin-api-token*.
   constant-time-equal folds length mismatch into the accumulator, so we do
   not pre-check length here — that would itself leak the token length."
  (let ((supplied (get-header request "x-admin-token")))
    (and supplied
         (constant-time-equal
          (sb-ext:string-to-octets supplied        :external-format :utf-8)
          (sb-ext:string-to-octets *admin-api-token* :external-format :utf-8)))))

(defun admin-path-p (path)
  "True for any path whose first segment is /admin."
  (and (>= (length path) 6)
       (string= path "/admin" :end1 6)
       (or (= (length path) 6)
           (char= (char path 6) #\/))))

;;; ---------------------------------------------------------------------------
;;; Response + body helpers
;;; ---------------------------------------------------------------------------

(defun make-json-response (status alist)
  (let ((resp (make-text-response status (json-serialize alist))))
    (set-response-header resp "content-type" "application/json; charset=utf-8")
    (set-response-header resp "cache-control" "no-store")
    resp))

(defun parse-json-body (request)
  "Parse the request body as JSON. Returns an alist on success, NIL on any
   failure (malformed JSON, non-object top-level, empty body)."
  (let ((body (http-request-body request)))
    (when (and body (> (length body) 0))
      (handler-case
          (let ((parsed (json-parse
                         (sb-ext:octets-to-string body :external-format :utf-8))))
            (when (listp parsed) parsed))
        (error () nil)))))

(defun json-string (obj key)
  "Like JSON-GET but only returns the value when it is a non-empty string."
  (let ((v (json-get obj key)))
    (and (stringp v) (> (length v) 0) v)))

;;; ---------------------------------------------------------------------------
;;; Outbound relay
;;; ---------------------------------------------------------------------------

(defun rs-auth-pairs ()
  `(("resource_server_id"     . ,*auth-rs-id*)
    ("resource_server_key_id" . ,*auth-rs-key-id*)
    ("resource_server_secret" . ,*auth-rs-secret*)))

(defun rs-relay (method path pairs)
  "Return a fetch continuation that relays to the auth-server's RS endpoint
   PATH. PAIRS is merged with the RS auth fields to form the JSON body.
   METHOD is :POST or :DELETE. The admin handler's return value IS this
   continuation — web-skeleton recognizes it and parks the inbound
   connection until the RS fetch resolves."
  (let* ((url  (concatenate 'string *auth-server-url* path))
         (body (json-serialize (append (rs-auth-pairs) pairs))))
    (defer-to-fetch method url
      :headers '(("content-type" . "application/json; charset=utf-8"))
      :body body
      :then (lambda (status headers body-bytes)
              (declare (ignore headers))
              (relay-upstream-response status body-bytes)))))

(defun relay-upstream-response (status body-bytes)
  "Forward the upstream status and (parsed) body to the admin caller. If the
   upstream didn't return parseable JSON we synthesise an error envelope so
   the admin caller always gets JSON. STATUS is NIL on the cleanup-
   sentinel path (upstream fetch aborted) — surface a 502 so the admin
   caller distinguishes abort from an upstream's own non-2xx reply."
  (when (null status)
    (log-warn "admin: RS relay fetch aborted")
    (return-from relay-upstream-response
      (make-json-response 502 '(("error" . "upstream fetch aborted")))))
  (let* ((text (if (null body-bytes)
                   ""
                   (handler-case (sb-ext:octets-to-string body-bytes
                                                          :external-format :utf-8)
                     (error () ""))))
         (json (handler-case (json-parse text) (error () nil))))
    (if (listp json)
        (make-json-response status json)
        (make-json-response (if (and (>= status 200) (< status 300)) 502 status)
                            `(("error" . "upstream returned non-JSON response")
                              ("upstream_status" . ,status))))))

;;; ---------------------------------------------------------------------------
;;; Endpoint implementations
;;; ---------------------------------------------------------------------------

(defun admin-invite-user (request)
  (let* ((body     (parse-json-body request))
         (username (json-string body "username"))
         (email    (json-string body "email")))
    (cond
      ((not (or username email))
       (make-json-response 400 '(("error" . "username or email required"))))
      (t
       (rs-relay :post "/api/rs/users"
                 (remove nil
                         (list (when username (cons "username" username))
                               (when email    (cons "email"    email)))))))))

(defun admin-lookup-user (request)
  (let* ((body    (parse-json-body request))
         (user-id (json-string body "user_id"))
         (uname   (json-string body "username"))
         (email   (json-string body "email")))
    (cond
      ((not (or user-id uname email))
       (make-json-response 400
                           '(("error" . "user_id, username, or email required"))))
      (t
       (rs-relay :post "/api/rs/users/lookup"
                 (remove nil
                         (list (when user-id (cons "user_id"  user-id))
                               (when uname   (cons "username" uname))
                               (when email   (cons "email"    email)))))))))

(defun admin-link-user (request)
  (let* ((body    (parse-json-body request))
         (user-id (json-string body "user_id")))
    (cond
      ((not user-id)
       (make-json-response 400 '(("error" . "user_id required"))))
      (t
       (rs-relay :post "/api/rs/client-users"
                 `(("client_id" . ,*auth-client-id*)
                   ("user_id"   . ,user-id)))))))

(defun admin-unlink-user (request)
  (let* ((body    (parse-json-body request))
         (user-id (json-string body "user_id")))
    (cond
      ((not user-id)
       (make-json-response 400 '(("error" . "user_id required"))))
      (t
       (rs-relay :delete "/api/rs/client-users"
                 `(("client_id" . ,*auth-client-id*)
                   ("user_id"   . ,user-id)))))))

(defun admin-list-client-users (request)
  (let* ((body   (parse-json-body request))
         (limit  (json-get body "limit"))
         (offset (json-get body "offset")))
    (rs-relay :post "/api/rs/client-users/list"
              (append `(("client_id" . ,*auth-client-id*))
                      (when (integerp limit)  `(("limit"  . ,limit)))
                      (when (integerp offset) `(("offset" . ,offset)))))))

;;; ---------------------------------------------------------------------------
;;; Router — called from handle-request for any /admin/* path.
;;; ---------------------------------------------------------------------------

(defun route-admin (request)
  (cond
    ((not (admin-enabled-p))
     (make-error-response 404))
    ((not (admin-auth-check request))
     ;; Return 404 (not 401) so admin endpoints are indistinguishable from
     ;; non-routes to unauthenticated callers.
     (make-error-response 404))
    (t
     (let ((path   (http-request-path request))
           (method (http-request-method request)))
       (cond
         ((and (string= path "/admin/users")             (eq method :POST))
          (admin-invite-user request))
         ((and (string= path "/admin/users/lookup")      (eq method :POST))
          (admin-lookup-user request))
         ((and (string= path "/admin/client-users")      (eq method :POST))
          (admin-link-user request))
         ((and (string= path "/admin/client-users")      (eq method :DELETE))
          (admin-unlink-user request))
         ((and (string= path "/admin/client-users/list") (eq method :POST))
          (admin-list-client-users request))
         (t (make-error-response 404)))))))
