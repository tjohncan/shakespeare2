(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; The generator seam: dispatch, the :NONE producer, and config validation.
;;;
;;; TEST-STREAM covers Ollama's framing by calling OLLAMA-GENERATE directly.
;;; This file covers the layer above it — which producer runs, what :NONE
;;; produces, and that a bad LLM_SERVICE is refused at startup rather than at
;;; the first poem.
;;; ===========================================================================

(defun %tokens-from (service)
  "Every token START-GENERATION yields under SERVICE, in order."
  (let ((seen nil)
        (shakespeare2::*llm-service* service))
    (shakespeare2::start-generation
     nil "a sonnet"
     :on-token (lambda (tk) (push tk seen))
     :on-done  (lambda (result) (declare (ignore result)) nil))
    (nreverse seen)))

(defun %done-status (service)
  "What ON-DONE was called with under SERVICE, or :NEVER if it was not called.

   :NEVER is the interesting answer. ON-DONE firing exactly once is what the
   handler's terminator hangs off — miss it and the client sits with an open
   output box and no EOT, which reads as a hung page rather than an error."
  (let ((result :never)
        (calls 0)
        (shakespeare2::*llm-service* service))
    (shakespeare2::start-generation
     nil "a sonnet"
     :on-token (lambda (tk) (declare (ignore tk)) nil)
     :on-done  (lambda (st) (incf calls) (setf result st)))
    (values result calls)))

(defun test-llm-completion ()
  "ON-DONE fires exactly once, with a status.

   The handler's EOT hangs off this callback, so a producer that forgets it
   leaves the client with an open output box and nothing to close it — a hung
   page rather than a visible failure. Counted rather than merely observed,
   because firing twice would send two terminators and read as fine here."
  (format t "~%LLM: completion callback~%")
  (multiple-value-bind (result calls) (%done-status :none)
    (check "llm: :none reports done"        result :ok)
    (check "llm: :none reports it once"     calls  1))
  ;; :OLLAMA pointed at nothing listening: the generation fails, and the
  ;; contract is that a failure is still an ending. A producer that only
  ;; called ON-DONE on success would hang the page on every upstream error.
  (let ((shakespeare2::*ollama-host* "127.0.0.1")
        (shakespeare2::*ollama-port* 1))
    (multiple-value-bind (result calls) (%done-status :ollama)
      (check "llm: a failed :ollama still reports done" result :failed)
      (check "llm: and reports it once"                 calls  1))))

(defun test-llm-dispatch ()
  "START-GENERATION routes to the configured producer, and refuses others.

   The :OLLAMA arm is asserted by where it fails, not by a happy path: with
   nothing listening it must reach OLLAMA-GENERATE and fail to connect,
   yielding no tokens and a :FAILED ending. A dispatch that fell through to
   :NONE would yield the disabled notice and :OK — a wrong answer that looks
   like a working one, which is what this pair exists to tell apart."
  (format t "~%LLM: dispatch~%")
  (check "llm: :none produces the disabled notice"
         (apply #'concatenate 'string (%tokens-from :none))
         shakespeare2::*disabled-message*)
  (let ((shakespeare2::*ollama-host* "127.0.0.1")
        (shakespeare2::*ollama-port* 1))
    (check "llm: :ollama yields nothing when there is no upstream"
           (%tokens-from :ollama) nil)
    (check "llm: :ollama reaches the Ollama producer, not :none"
           (%done-status :ollama) :failed))
  (check "llm: an unconfigured service is refused"
         (stringp (attempt (%tokens-from :nonesuch))) t))

(defun test-llm-none-producer ()
  "The :NONE producer streams, and streams the message exactly.

   Two properties, and the second is the one with teeth. Any tokenisation
   that dropped or invented a character would still produce a plausible
   stream — the client appends tokens, so it cannot tell — and the page would
   render subtly wrong text forever. Reassembling and comparing to the source
   is the only check that notices."
  (format t "~%LLM: the :none producer~%")
  (let ((tokens (%tokens-from :none)))
    ;; Streamed, not delivered whole. A producer that emitted one frame would
    ;; bypass the seam it is supposed to be an instance of.
    (check "llm: the notice arrives as many tokens, not one"
           (> (length tokens) 1) t)
    (check "llm: and reassembles to the message byte for byte"
           (apply #'concatenate 'string tokens)
           shakespeare2::*disabled-message*)))

(defun test-llm-split-into-tokens ()
  "SPLIT-INTO-TOKENS preserves its input exactly.

   Checked on inputs the message does not contain — leading whitespace, runs
   of it, a trailing newline, the empty string — because the producer above
   only ever exercises one string, and a splitter that happened to work on
   that one is not a splitter that works."
  (format t "~%LLM: token splitting~%")
  (dolist (s (list ""
                   "one"
                   "one two"
                   "  leading"
                   "trailing  "
                   (format nil "a~%~%b")
                   (format nil "~%")
                   (format nil "tabs~cand~cspaces " #\Tab #\Tab)))
    (check (format nil "llm: split preserves ~s" s)
           (apply #'concatenate 'string (shakespeare2::split-into-tokens s))
           s)))

(defun test-llm-service-config ()
  "LLM_SERVICE is parsed into a keyword, and a bad one fails at startup.

   The error is asserted to name the variable, not merely to be an error: a
   deployment that mistypes this reads one log line before the process exits,
   and 'must be one of' is the difference between a fix and a bisect."
  (format t "~%LLM: LLM_SERVICE parsing~%")
  (check "llm: ollama"       (shakespeare2::parse-llm-service "ollama") :ollama)
  (check "llm: none"         (shakespeare2::parse-llm-service "none")   :none)
  (check "llm: case-folded"  (shakespeare2::parse-llm-service "Ollama") :ollama)
  (check "llm: trimmed"      (shakespeare2::parse-llm-service " none ") :none)
  (let ((msg (attempt (shakespeare2::parse-llm-service "gpt"))))
    (check "llm: an unknown service is refused" (stringp msg) t)
    (check "llm: and the error names the variable"
           (and (stringp msg) (search "LLM_SERVICE" msg) t) t))
  (let ((msg (attempt (shakespeare2::parse-llm-service ""))))
    (check "llm: empty is refused too" (stringp msg) t)))

(defun %stop-after (service n)
  "Drive START-GENERATION under SERVICE, answering :STOP once N tokens have
   arrived. Returns (VALUES TOKENS RESULT CALLS)."
  (let ((seen nil)
        (result :never)
        (calls 0)
        (shakespeare2::*llm-service* service))
    (shakespeare2::start-generation
     nil "a sonnet"
     :on-token (lambda (tk)
                 (push tk seen)
                 (when (>= (length seen) n) :stop))
     :on-done  (lambda (st) (incf calls) (setf result st)))
    (values (nreverse seen) result calls)))

(defun test-llm-none-honours-a-stop ()
  "A producer that is told to stop stops, and says which ending it had.

   :STOP replaced a non-local exit that only worked while the producer was
   synchronous. On this backend it still is, which is what makes the claim
   checkable without a network: the tokens after the stop either arrive or
   they do not, in one call, on every run.

   :STOPPED rather than :OK is the half that carries downstream. The handler
   sends its terminator off this value, and a stop reported as a failure
   would turn every truncated poem into \"generation failed\" — a working
   feature reporting itself broken. Reported as :OK instead, the distinction
   the caller needs is simply gone.

   The last check is the non-vacuity one. If *DISABLED-MESSAGE* were two
   tokens long, a producer that ignored the stop entirely would deliver two
   tokens and pass the first check, so what makes the count mean anything is
   that there were more to refuse."
  (format t "~%LLM: a producer honours a stop~%")
  (multiple-value-bind (tokens result calls) (%stop-after :none 2)
    (check "llm stop: the producer stopped where it was told"
           (length tokens) 2)
    (check "llm stop: and reported the stop, not success" result :stopped)
    (check "llm stop: exactly once, as always" calls 1))
  (check "llm stop: there were more tokens to refuse"
         (> (length (%tokens-from :none)) 2) t))


(defun %ollama-seam (verdict)
  "Build OLLAMA-START's continuation without dialing, then drive its two
   callbacks by hand.

   Returns (values ON-BODY-ANSWER DONE): what :ON-BODY answered for one
   chunk carrying one real token line, and what ON-DONE was told when the
   fetch afterwards ended on the abort sentinel.

   FETCH-INTO is stubbed to capture the continuation rather than dial. That
   is the whole point: the claim is about a verdict crossing from ON-TOKEN
   to :ON-BODY, and a live server, a canned upstream and a socket are all
   downstream of it. Three attempts to test this end to end failed for
   reasons that were every one of them about the fixture — a flag set on
   both paths, a harness that answers at its deadline instead of raising,
   and a ten-second cost in that fixture nobody has explained. None of them
   were about the wire.

   The same move web-skeleton's own sticky test makes, for the same reason:
   stub the transport when the claim is a verdict and not a socket."
  (let ((captured nil)
        (done :never)
        (real (symbol-function 'web-skeleton:fetch-into)))
    (setf (symbol-function 'web-skeleton:fetch-into)
          (lambda (conn cont &key (failure-disposition :close))
            (declare (ignore conn failure-disposition))
            (setf captured cont)
            t))
    (unwind-protect
         (let ((conn (web-skeleton::make-connection :fd 1 :state :websocket)))
           (shakespeare2::ollama-start
            conn "a sonnet"
            (lambda (token) (declare (ignore token)) verdict)
            (lambda (result) (setf done result)))
           (let* ((line (format nil "~a~c" (first *ollama-corpus*) #\Newline))
                  (chunk (sb-ext:string-to-octets line :external-format :utf-8))
                  (answer (funcall (web-skeleton::http-fetch-continuation-on-body
                                    captured)
                                   nil chunk)))
             ;; The abort sentinel, which is what a stopped fetch and a dead
             ;; upstream both deliver. Which of the two this producer calls
             ;; it is the second half of the wire.
             (funcall (web-skeleton::http-fetch-continuation-callback captured)
                      nil nil nil)
             (values answer done)))
      (setf (symbol-function 'web-skeleton:fetch-into) real))))

(defun test-llm-ollama-relays-the-stop ()
  "ON-TOKEN's :STOP reaches :ON-BODY, and the ending is named for it.

   The one link nothing covered. Both sides of it were already tested: the
   cap answering :STOP has TEST-HANDLER-CAP-RETURNS-STOP, and :ON-BODY
   answering :STOP actually ending a fetch has a web-skeleton branch with
   fourteen reverts behind it. The wire between them had nothing — the
   pre-arranged mode exactly, where both ends read as covered.

   Four checks, because the wire has two directions and each needs its
   control. Without the NIL rows, an :ON-BODY hardcoded to :STOP and a
   producer hardcoded to :STOPPED both pass."
  (format t "~%LLM: the ollama arm relays a stop~%")
  (multiple-value-bind (answer done) (%ollama-seam :stop)
    (check "ollama stop: :on-body relayed the verdict" answer :stop)
    (check "ollama stop: and the ending is named a stop" done :stopped))
  (multiple-value-bind (answer done) (%ollama-seam nil)
    (check "ollama stop: no verdict, no relay" answer nil)
    (check "ollama stop: and the same sentinel is a failure"
           done :failed)))
(defun test-llm ()
  (test-llm-completion)
  (test-llm-dispatch)
  (test-llm-none-producer)
  (test-llm-none-honours-a-stop)
  (test-llm-ollama-relays-the-stop)
  (test-llm-split-into-tokens)
  (test-llm-service-config))
