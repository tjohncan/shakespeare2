(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Handler tests — origin validation + a live-server smoke test that drives
;;; the request path through web-skeleton-test-harness. Origin logic is
;;; pure and fast; the e2e pieces exercise the framework integration.
;;; ===========================================================================

(defun test-handler-same-origin-p ()
  (format t "~%Handler: same-origin-p~%")
  (check "exact match"
         (shakespeare2::same-origin-p "https://site.com" "site.com") t)
  (check "port preserved"
         (shakespeare2::same-origin-p "https://site.com:8443" "site.com:8443") t)
  (check "path ignored"
         (shakespeare2::same-origin-p "https://site.com/ignored" "site.com") t)
  (check "host mismatch"
         (shakespeare2::same-origin-p "https://other.com" "site.com") nil)
  (check "port mismatch"
         (shakespeare2::same-origin-p "https://site.com:443" "site.com") nil)
  (check "nil origin"
         (shakespeare2::same-origin-p nil "site.com") nil)
  (check "nil host"
         (shakespeare2::same-origin-p "https://site.com" nil) nil))

(defun test-handler-origin-allowed-p ()
  (format t "~%Handler: origin-allowed-p~%")
  ;; Fresh dynamic binding for *allowed-origins* so the test matrix is
  ;; independent of whatever load-config last set.
  (let ((shakespeare2::*allowed-origins* nil))
    (let ((no-origin     (make-test-request :path "/ws"))
          (same          (make-test-request :path "/ws"
                           :headers '(("host" . "site.com")
                                      ("origin" . "https://site.com"))))
          (cross         (make-test-request :path "/ws"
                           :headers '(("host" . "site.com")
                                      ("origin" . "https://evil.com")))))
      (check "no origin → permit (non-browser client)"
             (shakespeare2::origin-allowed-p no-origin) t)
      (check "same-origin (fallback, no allowlist)"
             (not (null (shakespeare2::origin-allowed-p same))) t)
      (check "cross-origin (fallback, no allowlist)"
             (shakespeare2::origin-allowed-p cross) nil)))
  ;; Explicit allowlist: Origin must match (case-insensitive) one of the
  ;; entries. Host header is ignored when an allowlist is set.
  (let ((shakespeare2::*allowed-origins* '("https://front.example"
                                            "https://admin.example")))
    (let ((allowed (make-test-request :path "/ws"
                     :headers '(("host" . "site.com")
                                ("origin" . "https://front.example"))))
          (denied  (make-test-request :path "/ws"
                     :headers '(("host" . "site.com")
                                ("origin" . "https://other.example")))))
      (check "allowlist match"
             (not (null (shakespeare2::origin-allowed-p allowed))) t)
      (check "allowlist miss"
             (shakespeare2::origin-allowed-p denied) nil))))

;;; ---------------------------------------------------------------------------
;;; Live-server smoke test — drives GET /healthz through the real event
;;; loop on an ephemeral port. This is the lowest-cost way to verify that
;;; route-request, serve-static, and the WS upgrade gate all wire together
;;; without any per-endpoint mocking.
;;; ---------------------------------------------------------------------------

(defun test-handler-healthz-e2e ()
  (format t "~%Handler: GET /healthz round-trip~%")
  (with-test-server (:handler #'shakespeare2::route-request)
    (multiple-value-bind (status headers body)
        (test-http-request :get "/healthz")
      (declare (ignore headers))
      (check "healthz status"
             status 200)
      (check "healthz body"
             body "ok"))))

(defun test-handler-ws-origin-reject-e2e ()
  (format t "~%Handler: WS upgrade with disallowed origin returns 403~%")
  ;; route-request calls origin-allowed-p with no allowlist → falls back
  ;; to same-origin comparison against Host. Passing a cross-origin
  ;; Origin header with a different Host triggers the 403 path. No actual
  ;; WS upgrade happens because we're using the HTTP client harness.
  (let ((shakespeare2::*allowed-origins* nil))
    (with-test-server (:handler #'shakespeare2::route-request)
      (multiple-value-bind (status headers body)
          (test-http-request :get "/ws"
                             :headers '(("host" . "site.com")
                                        ("origin" . "https://evil.com")))
        (declare (ignore headers body))
        (check "cross-origin WS upgrade rejected"
               status 403)))))

(defun test-handler ()
  (test-handler-same-origin-p)
  (test-handler-origin-allowed-p)
  (test-handler-healthz-e2e)
  (test-handler-ws-origin-reject-e2e))
