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


(defun %poem-frames (&key (max-chars 3000) (max-lines 40))
  "Every frame HANDLE-WS-MESSAGE sends for one poem request, in order.

   WS-SEND is stubbed rather than driven through a socket. The claim here is
   what the handler decides to send — how many token frames, and which
   terminator — and a real WebSocket would put a handshake, a frame decoder
   and a live connection between the decision and the assertion, any of which
   could be what broke. Restored in an UNWIND-PROTECT, because leaving the
   framework's writer replaced would break every test after this one in a way
   that looks like a framework fault.

   :NONE is the producer, so this runs to completion inside the call and the
   frames are all present when it returns."
  (let ((sent nil)
        (real (symbol-function 'web-skeleton:ws-send)))
    (setf (symbol-function 'web-skeleton:ws-send)
          (lambda (conn frame-bytes)
            (declare (ignore conn))
            (push frame-bytes sent)
            t))
    (unwind-protect
         (let ((shakespeare2::*llm-service* :none)
               (shakespeare2::*max-output-chars* max-chars)
               (shakespeare2::*max-output-lines* max-lines))
           (shakespeare2::handle-ws-message nil (%text-frame "a sonnet")))
      (setf (symbol-function 'web-skeleton:ws-send) real))
    (nreverse sent)))

(defun test-handler-output-cap ()
  "The output cap truncates the poem and still ends it normally.

   This path had no test at all until now — *MAX-OUTPUT-CHARS* appeared in
   the config and nowhere in this suite — which is how it stayed correct
   through a rewrite that changed what the cap does to the upstream.

   Two claims, and the second is the one the rewrite put at risk. A capped
   poem sends fewer frames than an uncapped one: the cap still stops output
   reaching the client. And it ends with EOT, not a NAK: a truncated poem is
   a normal ending in this app and always has been.

   The second is a real hazard rather than a hypothetical. The cap now stops
   the upstream by returning :STOP, and a stopped fetch ends through the
   framework's abort sentinel — the same NIL status a failed upstream
   delivers. Anything that mapped that to :FAILED would turn every truncated
   poem into \"generation failed\", so the producer answers :STOPPED and this
   is what notices if it stops.

   The uncapped run is the control. Without it, a handler that sent nothing
   at all would pass the frame-count check."
  (format t "~%Handler: the output cap~%")
  ;; The cap is derived from the message rather than written as a literal:
  ;; a shorter *DISABLED-MESSAGE* would otherwise make this vacuous, or
  ;; trip the cap on the first token and send no poem at all — which is how
  ;; the first version of this test failed its own guard below.
  (let ((capped (%poem-frames
                 :max-chars (floor (length shakespeare2::*disabled-message*) 2)))
        (whole  (%poem-frames)))
    (check "cap: a capped poem sends fewer frames than a whole one"
           (< (length capped) (length whole)) t)
    (check "cap: and more than just its terminators"
           (> (length capped) 2) t)
    ;; The discriminating one. Compared against the framework's own builder:
    ;; what is asserted is which terminator was sent, not what a text frame
    ;; looks like.
    (check "cap: a truncated poem still ends with EOT"
           (equalp (first (last capped))
                   (web-skeleton:build-ws-text shakespeare2::*eot*))
           t)
    (check "cap: an uncapped poem ends the same way"
           (equalp (first (last whole))
                   (web-skeleton:build-ws-text shakespeare2::*eot*))
           t)))

(defun %text-frame (text)
  (web-skeleton::make-ws-frame
   :fin t
   :opcode web-skeleton::+ws-op-text+
   :payload (sb-ext:string-to-octets text :external-format :utf-8)))

(defun %cap-verdicts (max-chars tokens)
  "What the handler's cap answers for each of TOKENS, in order.

   START-GENERATION is replaced by a producer this test drives, because the
   answer is the thing under test and no real producer would hand it back.
   Both stubs are restored in an UNWIND-PROTECT: leaving either in place
   would break the rest of the suite in a way that looks like a fault
   somewhere else."
  (let ((verdicts nil)
        (real-send (symbol-function 'web-skeleton:ws-send))
        (real-gen  (symbol-function 'shakespeare2::start-generation)))
    (setf (symbol-function 'web-skeleton:ws-send)
          (lambda (conn bytes) (declare (ignore conn bytes)) t))
    (setf (symbol-function 'shakespeare2::start-generation)
          (lambda (conn prompt &key on-token on-done)
            (declare (ignore conn prompt))
            (dolist (tk tokens)
              (push (funcall on-token tk) verdicts))
            (funcall on-done :ok)))
    (unwind-protect
         (let ((shakespeare2::*max-output-chars* max-chars)
               (shakespeare2::*max-output-lines* 40))
           (shakespeare2::handle-ws-message nil (%text-frame "a sonnet")))
      (setf (symbol-function 'shakespeare2::start-generation) real-gen)
      (setf (symbol-function 'web-skeleton:ws-send) real-send))
    (nreverse verdicts)))

(defun test-handler-cap-returns-stop ()
  "The cap answers :STOP, and keeps answering it.

   TEST-HANDLER-OUTPUT-CAP cannot see this, and the reason is worth stating:
   :OK and :STOPPED both send EOT, so a cap that stopped *sending* without
   stopping the *upstream* produces frames identical to a correct one. The
   whole point of the change is invisible from the client, which is what
   makes it invisible to a test that watches the client.

   So this watches the verdict instead — the one value the handler
   contributes to the seam. Four tokens of five characters against a cap of
   twelve: two fit, and everything from the third on is refused.

   The tail matters as much as the transition. :STOP ends the fetch and not
   the pass, so tokens already decoded keep arriving after the verdict is
   given; answering NIL for those would put this handler's correctness inside
   a framework detail it does not own. The expected list is written out in
   full rather than checked for a first :STOP, because \"stops at the right
   token\" and \"keeps saying so\" are two claims and one list holds both."
  (format t "~%Handler: the cap's verdict~%")
  (check "cap verdict: two fit, the rest are refused"
         (%cap-verdicts 12 '("aaaa " "bbbb " "cccc " "dddd "))
         '(nil nil :stop :stop))
  ;; The control: with room for all four, nothing is refused. Without it, a
  ;; cap that answered :STOP unconditionally would pass the check above at
  ;; every position but the first two.
  (check "cap verdict: an uncapped run refuses nothing"
         (%cap-verdicts 1000 '("aaaa " "bbbb " "cccc " "dddd "))
         '(nil nil nil nil)))
(defun test-handler ()
  (test-handler-same-origin-p)
  (test-handler-origin-allowed-p)
  (test-handler-healthz-e2e)
  (test-handler-ws-origin-reject-e2e)
  (test-handler-output-cap)
  (test-handler-cap-returns-stop))
