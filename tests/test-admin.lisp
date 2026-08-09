(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Admin relay tests.
;;;
;;; The admin path has two failure modes and they need different treatment.
;;; A raise from JSON-SERIALIZE becomes a 500 and any smoke test finds it.
;;; A shape-test that quietly answers NIL does not: the endpoint returns a
;;; well-formed 400 or 502 that names something else as the cause. These
;;; tests exist for the second kind, so they assert on the answer a caller
;;; receives rather than on the absence of an error.
;;;
;;; None of this needs a network. RS-RELAY builds a continuation and returns
;;; it; the framework is what would dial out. So the whole relay can be
;;; driven in-process, including the body it would have sent.
;;; ===========================================================================

(defmacro with-admin-config (&body body)
  "Bind the RS credentials and client id the relay reads. Fixed values so
   the expected request bodies below are exact."
  `(let ((shakespeare2::*auth-rs-id*      "rs-1")
         (shakespeare2::*auth-rs-key-id*  "key-1")
         (shakespeare2::*auth-rs-secret*  "secret-1")
         (shakespeare2::*auth-client-id*  "client-1")
         (shakespeare2::*auth-server-url* "https://auth.example"))
     ,@body))

(defun admin-request (body &key (method :POST) (path "/admin/x"))
  (make-test-request :method method :path path :body body))

(defun relayed-body (continuation)
  "The JSON the relay would have PUT on the wire. Reaches for the
   framework's slot accessor directly — the continuation is deliberately
   opaque to handlers, and this is a test asserting a wire contract, which
   is the one caller that has business looking inside it."
  (web-skeleton::http-fetch-continuation-body continuation))

(defun continuation-p (x)
  (typep x 'web-skeleton::http-fetch-continuation))

;;; ---------------------------------------------------------------------------
;;; Body parsing — the shape test that decides whether any endpoint sees
;;; its own request.
;;; ---------------------------------------------------------------------------

(defun test-admin-parse-json-body ()
  (format t "~%Admin: parse-json-body~%")
  (let ((parsed (shakespeare2::parse-json-body
                 (admin-request "{\"user_id\":\"u-123\",\"n\":7}"))))
    (check "well-formed object is accepted"
           (json-object-p parsed) t)
    (check "string field reads back"
           (json-get parsed "user_id") "u-123")
    (check "integer field reads back"
           (json-get parsed "n") 7))
  (check "empty object is accepted (and is not NIL)"
         (json-object-p
          (shakespeare2::parse-json-body (admin-request "{}"))) t)
  ;; A top-level array is not a request body this API accepts. It is also
  ;; the input a LISTP shape test would have waved through.
  (check "top-level array is rejected"
         (shakespeare2::parse-json-body (admin-request "[1,2]")) nil)
  (check "top-level string is rejected"
         (shakespeare2::parse-json-body (admin-request "\"nope\"")) nil)
  (check "malformed JSON is rejected"
         (shakespeare2::parse-json-body (admin-request "{oops")) nil)
  (check "empty body is rejected"
         (shakespeare2::parse-json-body (admin-request "")) nil)
  (check "absent body is rejected"
         (shakespeare2::parse-json-body (admin-request nil)) nil))

(defun test-admin-json-string ()
  (format t "~%Admin: json-string helper~%")
  ;; Fed what PARSE-JSON-BODY actually returns. Handing this a hand-written
  ;; alist would exercise the framework's compatibility shim instead of the
  ;; shape this app receives, and would pass whether or not parsing works.
  (let ((obj (shakespeare2::parse-json-body
              (admin-request "{\"s\":\"val\",\"empty\":\"\",\"n\":1}"))))
    (check "string value"        (shakespeare2::json-string obj "s") "val")
    (check "empty string rejected" (shakespeare2::json-string obj "empty") nil)
    (check "integer rejected"    (shakespeare2::json-string obj "n") nil)
    (check "missing key"         (shakespeare2::json-string obj "absent") nil))
  (check "nil object"            (shakespeare2::json-string nil "s") nil))

;;; ---------------------------------------------------------------------------
;;; Response construction
;;; ---------------------------------------------------------------------------

(defun test-admin-make-json-response ()
  (format t "~%Admin: make-json-response~%")
  (let ((r (shakespeare2::make-json-response
            400 '(("error" . "user_id required")))))
    (check "bare alist: status"  (http-response-status r) 400)
    (check "bare alist: body"    (http-response-body r)
           "{\"error\":\"user_id required\"}")
    (check "content-type set"
           (cdr (assoc "content-type" (http-response-headers r) :test #'string=))
           "application/json; charset=utf-8")
    (check "cache-control set"
           (cdr (assoc "cache-control" (http-response-headers r) :test #'string=))
           "no-store"))
  ;; The upstream-passthrough caller hands this an already-parsed object.
  (let ((r (shakespeare2::make-json-response
            200 (json-parse "{\"user_id\":\"u-123\"}"))))
    (check "json-object: status" (http-response-status r) 200)
    (check "json-object: body not double-wrapped"
           (http-response-body r) "{\"user_id\":\"u-123\"}")))

;;; ---------------------------------------------------------------------------
;;; Upstream relay — forwarding auth-server's answer back to the operator.
;;; ---------------------------------------------------------------------------

(defun utf8 (s) (sb-ext:string-to-octets s :external-format :utf-8))

(defun test-admin-relay-upstream-response ()
  (format t "~%Admin: relay-upstream-response~%")
  ;; The case that matters: upstream succeeded and returned a JSON object.
  ;; A shape test that misses here converts every working call into a 502
  ;; blaming auth-server.
  (let ((r (shakespeare2::relay-upstream-response
            200 (utf8 "{\"user_id\":\"u-123\",\"created\":true}"))))
    (check "success passes the status through" (http-response-status r) 200)
    (check "success passes the body through"
           (http-response-body r) "{\"user_id\":\"u-123\",\"created\":true}"))
  (let ((r (shakespeare2::relay-upstream-response
            201 (utf8 "{\"ok\":true}"))))
    (check "non-200 2xx passes through too" (http-response-status r) 201))
  ;; Upstream's own error replies are JSON as well, and must not be
  ;; relabelled on the way back.
  (let ((r (shakespeare2::relay-upstream-response
            409 (utf8 "{\"error\":\"already_linked\"}"))))
    (check "upstream 4xx keeps its status" (http-response-status r) 409)
    (check "upstream 4xx keeps its body"
           (http-response-body r) "{\"error\":\"already_linked\"}"))
  ;; Genuinely non-JSON from a 2xx is the only case that earns a synthesised
  ;; 502 — the upstream claimed success in a form this relay cannot forward.
  (let ((r (shakespeare2::relay-upstream-response 200 (utf8 "<html>hi"))))
    (check "non-JSON 2xx becomes 502" (http-response-status r) 502)
    (check "non-JSON 2xx explains itself"
           (http-response-body r)
           "{\"error\":\"upstream returned non-JSON response\",\"upstream_status\":200}"))
  ;; An auth-server running on web-skeleton answers its connection limit
  ;; with a plain-text 503. That is non-JSON, but it is not a 2xx, so the
  ;; status is forwarded rather than replaced — the operator sees the
  ;; overload for what it is.
  (let ((r (shakespeare2::relay-upstream-response
            503 (utf8 "Service Unavailable"))))
    (check "upstream 503 is forwarded, not masked as 502"
           (http-response-status r) 503))
  (let ((r (shakespeare2::relay-upstream-response 200 nil)))
    (check "empty body from a 2xx becomes 502" (http-response-status r) 502))
  ;; STATUS NIL is the framework's cleanup sentinel: the fetch never
  ;; resolved. Distinct from any answer the upstream could have given.
  (let ((r (shakespeare2::relay-upstream-response nil nil)))
    (check "aborted fetch is 502" (http-response-status r) 502)
    (check "aborted fetch says so"
           (http-response-body r) "{\"error\":\"upstream fetch aborted\"}")))

;;; ---------------------------------------------------------------------------
;;; End to end through the endpoints — what an operator's curl gets back.
;;; ---------------------------------------------------------------------------

(defun test-admin-endpoints ()
  (format t "~%Admin: endpoints relay valid requests~%")
  (with-admin-config
    (let ((r (shakespeare2::admin-link-user
              (admin-request "{\"user_id\":\"u-123\"}"))))
      (check "link-user relays rather than rejecting" (continuation-p r) t)
      (check "link-user body carries RS auth and the user"
             (and (continuation-p r) (relayed-body r))
             "{\"resource_server_id\":\"rs-1\",\"resource_server_key_id\":\"key-1\",\"resource_server_secret\":\"secret-1\",\"client_id\":\"client-1\",\"user_id\":\"u-123\"}"))
    (let ((r (shakespeare2::admin-invite-user
              (admin-request "{\"username\":\"tiger\"}"))))
      (check "invite-user relays" (continuation-p r) t)
      (check "invite-user body carries the username"
             (and (continuation-p r) (relayed-body r))
             "{\"resource_server_id\":\"rs-1\",\"resource_server_key_id\":\"key-1\",\"resource_server_secret\":\"secret-1\",\"username\":\"tiger\"}"))
    (let ((r (shakespeare2::admin-lookup-user
              (admin-request "{\"email\":\"a@b.c\"}"))))
      (check "lookup-user relays" (continuation-p r) t))
    (let ((r (shakespeare2::admin-unlink-user
              (admin-request "{\"user_id\":\"u-123\"}"))))
      (check "unlink-user relays" (continuation-p r) t))
    ;; This endpoint has no required field, so a body that fails to parse
    ;; does not produce a 400 — it relays with the pagination silently
    ;; dropped and the operator gets page one of an unbounded list.
    (let ((r (shakespeare2::admin-list-client-users
              (admin-request "{\"limit\":5,\"offset\":10}"))))
      (check "list-client-users relays" (continuation-p r) t)
      (check "list-client-users forwards limit and offset"
             (and (continuation-p r) (relayed-body r))
             "{\"resource_server_id\":\"rs-1\",\"resource_server_key_id\":\"key-1\",\"resource_server_secret\":\"secret-1\",\"client_id\":\"client-1\",\"limit\":5,\"offset\":10}"))

    (format t "~%Admin: endpoints still reject genuinely empty requests~%")
    (let ((r (shakespeare2::admin-link-user (admin-request "{}"))))
      (check "no user_id is still a 400" (http-response-status r) 400)
      (check "400 names the missing field"
             (http-response-body r) "{\"error\":\"user_id required\"}"))
    (let ((r (shakespeare2::admin-invite-user (admin-request "{\"other\":1}"))))
      (check "no username or email is still a 400"
             (http-response-status r) 400))
    ;; Non-integer pagination is ignored rather than forwarded — the
    ;; INTEGERP guards, not the parse, are what reject it.
    (let ((r (shakespeare2::admin-list-client-users
              (admin-request "{\"limit\":\"5\"}"))))
      (check "string limit is dropped, not relayed"
             (and (continuation-p r) (relayed-body r))
             "{\"resource_server_id\":\"rs-1\",\"resource_server_key_id\":\"key-1\",\"resource_server_secret\":\"secret-1\",\"client_id\":\"client-1\"}"))))

(defun test-admin ()
  (test-admin-parse-json-body)
  (test-admin-json-string)
  (test-admin-make-json-response)
  (test-admin-relay-upstream-response)
  (test-admin-endpoints))
