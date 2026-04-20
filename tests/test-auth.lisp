(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Auth tests (#+shakespeare2/auth only).
;;;
;;; Focus on the pure helpers that the /authorize → /callback flow leans
;;; on — URL encoding, form body building, path classifiers, the reaper
;;; predicate — plus a round-trip through the session store using the
;;; framework's make-store primitive. The full OAuth2 flow needs a live
;;; auth-server and isn't mocked here; introspection and token exchange
;;; are covered by manual end-to-end testing against a real deploy.
;;; ===========================================================================

(defun test-auth-path-classifiers ()
  (format t "~%Auth: path classifiers~%")
  (check "public /callback"
         (shakespeare2::public-path-p "/callback") t)
  (check "public /logout"
         (shakespeare2::public-path-p "/logout") t)
  (check "public /healthz"
         (shakespeare2::public-path-p "/healthz") t)
  (check "non-public /"
         (shakespeare2::public-path-p "/") nil)
  (check "non-public /ws"
         (shakespeare2::public-path-p "/ws") nil)
  ;; public-asset-path-p covers ungated static assets loaded by <link>
  ;; tags without credentials (favicons, manifest, robots.txt).
  (check "asset /favicon.ico"
         (shakespeare2::public-asset-path-p "/favicon.ico") t)
  (check "asset /robots.txt"
         (shakespeare2::public-asset-path-p "/robots.txt") t)
  (check "asset /favicon/site.webmanifest"
         (shakespeare2::public-asset-path-p "/favicon/site.webmanifest") t)
  (check "asset /favicon/favicon.svg"
         (shakespeare2::public-asset-path-p "/favicon/favicon.svg") t)
  (check "asset reject /faviconX"
         (shakespeare2::public-asset-path-p "/faviconX") nil)
  (check "asset reject /"
         (shakespeare2::public-asset-path-p "/") nil)
  ;; admin-path-p must match /admin and /admin/* but not a prefix like
  ;; /administrator (which would route-leak the admin surface).
  (check "admin /admin"
         (shakespeare2::admin-path-p "/admin") t)
  (check "admin /admin/users"
         (shakespeare2::admin-path-p "/admin/users") t)
  (check "admin reject /administrator"
         (shakespeare2::admin-path-p "/administrator") nil)
  (check "admin reject /adm"
         (shakespeare2::admin-path-p "/adm") nil)
  (check "admin reject /"
         (shakespeare2::admin-path-p "/") nil))

(defun test-auth-session-doomed-p ()
  (format t "~%Auth: session-doomed-p~%")
  (let ((now (get-universal-time))
        (stale (- (get-universal-time)
                  (1+ shakespeare2::*session-stale-seconds*))))
    (check "live authenticated session"
           (shakespeare2::session-doomed-p
            "id" (list :access-token "tok"
                       :expires-at (+ now 3600)
                       :created-at now))
           nil)
    (check "expired authenticated session"
           (not (null (shakespeare2::session-doomed-p
                       "id" (list :access-token "tok"
                                  :expires-at (- now 1)
                                  :created-at stale))))
           t)
    (check "fresh pre-auth session"
           (shakespeare2::session-doomed-p
            "id" (list :created-at now))
           nil)
    (check "stalled pre-auth session"
           (not (null (shakespeare2::session-doomed-p
                       "id" (list :created-at stale))))
           t)
    (check "session missing created-at treated as epoch (reapable)"
           (not (null (shakespeare2::session-doomed-p
                       "id" (list))))
           t)))

(defun test-auth-url-encode ()
  (format t "~%Auth: url-encode~%")
  (check "unreserved ASCII pass through"
         (shakespeare2::url-encode "AZaz09-._~")
         "AZaz09-._~")
  (check "space percent-encoded"
         (shakespeare2::url-encode "hello world")
         "hello%20world")
  (check "ampersand percent-encoded"
         (shakespeare2::url-encode "a&b")
         "a%26b")
  (check "plus is NOT unreserved (stays encoded)"
         (shakespeare2::url-encode "a+b")
         "a%2Bb")
  (check "utf-8 multibyte"
         (shakespeare2::url-encode (format nil "café"))
         "caf%C3%A9"))

(defun test-auth-build-form-body ()
  (format t "~%Auth: build-form-body~%")
  (check "single pair"
         (shakespeare2::build-form-body '(("k" . "v")))
         "k=v")
  (check "multiple pairs ordered"
         (shakespeare2::build-form-body '(("a" . "1") ("b" . "2") ("c" . "3")))
         "a=1&b=2&c=3")
  (check "nil values dropped"
         (shakespeare2::build-form-body '(("a" . "1") ("b" . nil) ("c" . "3")))
         "a=1&c=3")
  (check "integer value coerced"
         (shakespeare2::build-form-body '(("n" . 42)))
         "n=42")
  (check "empty pairs produces empty string"
         (shakespeare2::build-form-body nil)
         "")
  (check "value needing encoding"
         (shakespeare2::build-form-body '(("msg" . "hello world")))
         "msg=hello%20world"))

(defun test-auth-json-string ()
  (format t "~%Auth: admin json-string helper~%")
  (check "string value"
         (shakespeare2::json-string '(("k" . "val")) "k") "val")
  (check "empty string rejected"
         (shakespeare2::json-string '(("k" . "")) "k") nil)
  (check "integer rejected"
         (shakespeare2::json-string '(("k" . 1)) "k") nil)
  (check "missing key"
         (shakespeare2::json-string '() "k") nil)
  (check "nil object"
         (shakespeare2::json-string nil "k") nil))

;;; ---------------------------------------------------------------------------
;;; Session store round-trip — exercises web-skeleton's MAKE-STORE +
;;; STORE-UPDATE-PLIST in the same shape shakespeare2 uses at runtime.
;;; Bypasses INIT-SESSIONS to avoid spawning the reaper thread from inside
;;; the test (would need explicit teardown); binds *SESSIONS* to a fresh
;;; store without the reaper (no :expiry-fn / :reap-interval).
;;; ---------------------------------------------------------------------------

(defun test-auth-session-store-roundtrip ()
  (format t "~%Auth: *sessions* store round-trip~%")
  (let ((shakespeare2::*sessions* (make-store)))
    (let ((id (random-token)))
      (store-set shakespeare2::*sessions* id
                 (list :created-at (get-universal-time)))
      (check "initial session has created-at"
             (integerp (getf (store-get shakespeare2::*sessions* id) :created-at))
             t)
      (store-update-plist shakespeare2::*sessions* id
                          :access-token "tok"
                          :expires-at   (+ (get-universal-time) 3600))
      (let ((sess (store-get shakespeare2::*sessions* id)))
        (check "access-token set"   (getf sess :access-token) "tok")
        (check "expires-at present" (integerp (getf sess :expires-at)) t)
        (check "session-live-p T"   (not (null (shakespeare2::session-live-p sess))) t))
      (store-delete shakespeare2::*sessions* id)
      (check "session gone after delete"
             (store-get shakespeare2::*sessions* id) nil))))

(defun test-auth ()
  (test-auth-path-classifiers)
  (test-auth-session-doomed-p)
  (test-auth-url-encode)
  (test-auth-build-form-body)
  (test-auth-json-string)
  (test-auth-session-store-roundtrip))
