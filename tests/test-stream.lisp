(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Streaming-path tests — STREAM-GENERATE against a canned upstream.
;;;
;;; The payload tests next door assert the bytes that go *out*. Nothing
;;; asserted what happens to the bytes that come *back*, which is the app's
;;; whole purpose: OLLAMA-PAYLOAD exists as a separate function precisely so
;;; the request could be checked "without a running model", and the reply half
;;; was left uncovered because checking it needed one.
;;;
;;; A canned upstream removes that requirement, and removes the model from
;;; development entirely — no GPU, no pull, no daemon. It is also a better
;;; fixture than a live model, for the same reason web-skeleton's chunked
;;; tests hand-write their corpus: the *boundaries* become the test's choice
;;; instead of whatever the generator happened to emit that run.
;;;
;;; ---------------------------------------------------------------------------
;;; Where the corpus came from, and why that mattered
;;;
;;; Captured from a real Ollama /api/generate response rather than written
;;; from a description of one, and that is the whole reason these bytes can
;;; be trusted. The description available at the time was wrong in a way
;;; that reads as right: it had the summary object *missing* its "response"
;;; field, where the real one carries "response":"" beside "done":true. A
;;; corpus written from it would have omitted the key and then proved the
;;; guard handles a shape Ollama never sends — green, and asserting a
;;; counterfactual. STREAM-GENERATE's docstring now describes what is here.
;;;
;;; So: if this corpus ever needs regenerating, capture it. Do not write it
;;; from this file, from that docstring, or from memory of either.
;;;
;;; To re-capture, with any model pulled:
;;;
;;;   curl -s --raw -i http://localhost:11434/api/generate \
;;;     -H "content-type: application/json" \
;;;     -d '{"model":"MODEL","system":"be bard","prompt":"say hello",
;;;          "stream":true,"options":{"num_predict":6}}'
;;;
;;; --raw is the load-bearing flag: without it curl decodes the chunked
;;; framing before you see it, which is the half being reproduced here.
;;; ===========================================================================

(defparameter *ollama-corpus*
  (list
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0133448Z\",\"response\":\"Hello\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0263412Z\",\"response\":\"!\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0359767Z\",\"response\":\" How\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0455739Z\",\"response\":\" can\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0550502Z\",\"response\":\" I\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0647608Z\",\"response\":\" help\",\"done\":false}"
   "{\"model\":\"qwen2.5:0.5b\",\"created_at\":\"2026-08-30T23:40:05.0647608Z\",\"response\":\"\",\"done\":true,\"done_reason\":\"length\",\"context\":[151644,8948,198,1371,41810,151645,198,151644,872,198,36790,23811,151645,198,151644,77091,198,9707,0,2585,646,358,1492],\"total_duration\":695813600,\"load_duration\":599113800,\"prompt_eval_count\":17,\"prompt_eval_duration\":42875800,\"eval_count\":6,\"eval_duration\":42891000}")
  "One real /api/generate response, one JSON object per element, in order.
   The last is the summary object: response is present and empty, done true.")

(defparameter *ollama-expected-tokens*
  '("Hello" "!" " How" " can" " I" " help")
  "What the corpus above must yield. The summary object contributes nothing.")

;;; ---------------------------------------------------------------------------
;;; The fixture
;;; ---------------------------------------------------------------------------

(defun %read-request (stream)
  "Read one HTTP request off STREAM: head, then body if a Content-Length says
   there is one.

   Reading to end-of-stream would deadlock both sides — the client sends its
   request and then waits, so no EOF is coming until we answer.

   Reading only to CRLFCRLF is not enough either, and that is the one
   difference from web-skeleton's GET-shaped fixture this is modelled on:
   STREAM-GENERATE POSTs a JSON body. Answering before that body is consumed
   leaves the client writing into a socket this thread is about to close, and
   the failure surfaces as an error from inside the framework rather than as
   anything naming the fixture."
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8)
                              :fill-pointer 0 :adjustable t)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buf)
          until (web-skeleton::scan-crlf-crlf buf 0 (fill-pointer buf)))
    (let* ((head (sb-ext:octets-to-string
                  (coerce buf '(vector (unsigned-byte 8)))
                  :external-format :latin-1))
           (marker (search "content-length:" (string-downcase head)))
           (declared (when marker
                       (parse-integer head :start (+ marker 15)
                                           :junk-allowed t))))
      (when (and declared (plusp declared))
        (dotimes (i declared)
          (declare (ignorable i))
          (unless (read-byte stream nil nil) (return)))))
    t))

(defun %canned-ollama (listener response &key (name "canned-ollama"))
  "Accept once, read the whole request, write RESPONSE verbatim, close.
   Returns the thread. RESPONSE is a complete HTTP response, status line and
   headers included, so a test chooses the framing as well as the body."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let* ((sock (sb-bsd-sockets:socket-accept listener))
                (stream (sb-bsd-sockets:socket-make-stream
                         sock :input t :output t
                              :element-type '(unsigned-byte 8))))
           (%read-request stream)
           (write-sequence
            (sb-ext:string-to-octets response :external-format :utf-8) stream)
           (force-output stream)
           (sb-bsd-sockets:socket-close sock))
       (error () nil)))
   :name name))

(defun %call-with-canned-ollama (response fn)
  "Run FN with *OLLAMA-HOST* and *OLLAMA-PORT* pointed at a canned upstream
   serving RESPONSE.

   SETF rather than LET, and restored in the UNWIND-PROTECT. The producer now
   runs on a worker thread that WITH-TEST-SERVER spawned, and dynamic bindings
   do not cross MAKE-THREAD — a LET here would leave the worker reading the
   real configuration and dialling a host that is not there. The blocking
   version could use LET because it ran on this thread; the port is what
   changed that."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp))
        (saved-host shakespeare2::*ollama-host*)
        (saved-port shakespeare2::*ollama-port*)
        (thread nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 5)
           (multiple-value-bind (host port) (sb-bsd-sockets:socket-name listener)
             (declare (ignore host))
             (setf thread (%canned-ollama listener response))
             (setf shakespeare2::*ollama-host* "127.0.0.1"
                   shakespeare2::*ollama-port* port)
             (funcall fn)))
      (setf shakespeare2::*ollama-host* saved-host
            shakespeare2::*ollama-port* saved-port)
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when thread
        (handler-case (sb-thread:join-thread thread :timeout 5)
          (error () (ignore-errors (sb-thread:terminate-thread thread))))))))

(defun %chunked (&rest pieces)
  "PIECES as chunked body data, one chunk per piece, terminator included.
   Hand-framed so a test can put a chunk boundary wherever it likes —
   including the middle of a JSON object, which no live model will do for you."
  (with-output-to-string (out)
    (dolist (p pieces)
      (format out "~x~c~c~a~c~c"
              (length (sb-ext:string-to-octets p :external-format :utf-8))
              #\Return #\Newline p #\Return #\Newline))
    (format out "0~c~c~c~c" #\Return #\Newline #\Return #\Newline)))

(defun %ollama-response (&rest chunks)
  "A complete chunked HTTP response carrying CHUNKS, headers as Ollama sends
   them: application/x-ndjson, Transfer-Encoding chunked."
  (let ((crlf (format nil "~c~c" #\Return #\Newline)))
    (concatenate 'string
                 "HTTP/1.1 200 OK" crlf
                 "Content-Type: application/x-ndjson" crlf
                 "Transfer-Encoding: chunked" crlf
                 crlf
                 (apply #'%chunked chunks))))

(defun %ollama-response-cut-short (&rest chunks)
  "CHUNKS with the headers, and then nothing — no zero-size terminator, the
   connection simply ends.

   What an upstream that dies mid-generation looks like on the wire: a model
   container killed, a machine going away. The framing says more is coming
   and no more comes, which is the one shape a relay must not report as a
   finished poem."
  (let ((crlf (format nil "~c~c" #\Return #\Newline)))
    (concatenate 'string
                 "HTTP/1.1 200 OK" crlf
                 "Content-Type: application/x-ndjson" crlf
                 "Transfer-Encoding: chunked" crlf
                 crlf
                 (with-output-to-string (out)
                   (dolist (p chunks)
                     (format out "~x~c~c~a~c~c"
                             (length (sb-ext:string-to-octets
                                      p :external-format :utf-8))
                             #\Return #\Newline p #\Return #\Newline))))))

(defun %line-chunks (lines)
  "LINES as Ollama frames them: one chunk per object, each ending in LF.
   The LF is payload — ndjson — and is not the chunk's own CRLF terminator."
  (mapcar (lambda (l) (format nil "~a~c" l #\Newline)) lines))

(defun %relay-tokens (response)
  "Drive OLLAMA-START against a canned upstream serving RESPONSE, through a
   real server and a real streaming connection, and return (VALUES TOKENS
   DONE-STATUS).

   The whole path: FETCH-INTO attaches an outbound to a connection the app
   owns, :ON-BODY delivers chunks, the accumulator rejoins lines, and
   OLLAMA-TOKEN-FROM-LINE reads them. Nothing here is a stand-in — the
   producer runs on a worker thread, driven by the event loop, exactly as it
   does in production.

   A live server is the cheapest honest way to reach FETCH-INTO. It needs a
   connection in a state that owns its own write path and an event loop to
   attach to; there is no useful smaller fixture. A streaming response is used
   rather than a WebSocket because the framework accepts either and this one
   needs no handshake client.

   ATTEMPT wraps the run rather than the call, so a raise from inside the
   producer becomes a failed check rather than the end of the suite."
  (let ((seen nil)
        (done :never))
    (let ((raised
            (attempt
             (%call-with-canned-ollama
              response
              (lambda ()
                (with-test-server
                    (:handler
                     (lambda (req)
                       (declare (ignore req))
                       (make-stream-response
                        :on-open
                        (lambda (client)
                          (shakespeare2::ollama-start
                           client "a sonnet"
                           (lambda (token) (push token seen))
                           (lambda (status)
                             (setf done status)
                             ;; Ends the response, which is what lets
                             ;; TEST-HTTP-REQUEST below return — so by the
                             ;; time this function does, generation is over
                             ;; and SEEN is complete.
                             (stream-close client)))))))
                  (test-http-request :get "/relay")))))))
      (if (stringp raised)
          (values raised done)
          (values (nreverse seen) done)))))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(defun test-stream-relay-e2e ()
  "The whole relay: a canned upstream, through FETCH-INTO, into a live
   connection, with the tokens coming out the far end in order.

   One test rather than three, because the parts the three used to isolate
   now have their own detectors that need no socket: line reassembly in
   TEST-LINES, and what a line means in TEST-STREAM-TOKEN-FROM-LINE. What is
   left for an end-to-end check is the thing only it can see — that the parts
   are wired to each other and to the event loop.

   The corpus is deliberately hostile in three ways at once, all of which a
   live Ollama would refuse to produce:

     an object split across two chunks   the reassembly's whole reason; Ollama
                                         emits one object per chunk, so a real
                                         upstream never exercises it
     a blank line                        defence, not an observed shape
     an unparseable line                 the guard in OLLAMA-TOKEN-FROM-LINE

   The last token is the assertion with teeth. Any of those three handled by
   aborting rather than skipping would drop everything after it, and a check
   that only looked for the absence of junk would not notice."
  (format t "~%Stream: the relay, end to end~%")
  (let* ((whole (first *ollama-corpus*))
         (cut   (floor (length whole) 2))
         (rest-of-corpus (cdr *ollama-corpus*)))
    (multiple-value-bind (tokens done)
        (%relay-tokens
         (apply #'%ollama-response
                (append
                 ;; The first object, cut in half across two chunks.
                 (list (subseq whole 0 cut)
                       (format nil "~a~c" (subseq whole cut) #\Newline))
                 ;; Then junk, then the rest of the real response.
                 (%line-chunks (cons "" (cons "{not json at all"
                                              rest-of-corpus))))))
      (check "relay: every token arrives, in order"
             tokens *ollama-expected-tokens*)
      ;; Separately, because the list check would also pass if the summary
      ;; object contributed something that compared equal to nothing.
      (check "relay: the summary object contributes no token"
             (length tokens) (length *ollama-expected-tokens*))
      (check "relay: and the generation reports a clean ending"
             done :ok))))

(defun test-stream-token-from-line ()
  "OLLAMA-TOKEN-FROM-LINE, against the captured corpus, with no transport.

   This is the detector meant to survive the relay port. Everything else in
   this file goes through a socket and a canned upstream, and all of that
   plumbing is replaced when the transport changes — so those checks change
   in the same commit as the code they cover, which is the one arrangement
   where a revert-check is easiest to fool.

   What a line *means* does not change when the way it arrives does. Pinning
   that here, on the real bytes and with nothing else in the way, leaves one
   assertion in the port's blast radius that the port cannot quietly move."
  (format t "~%Stream: what a line means, without a transport~%")
  (check "line: the corpus yields exactly its tokens"
         (remove nil (mapcar #'shakespeare2::ollama-token-from-line
                             *ollama-corpus*))
         *ollama-expected-tokens*)
  ;; The summary object named individually, because it is the one whose shape
  ;; the docstring got wrong: present-and-empty, not missing.
  (check "line: the summary object yields nothing"
         (shakespeare2::ollama-token-from-line (car (last *ollama-corpus*)))
         nil)
  (check "line: a blank line yields nothing"
         (shakespeare2::ollama-token-from-line "") nil)
  (check "line: unparseable yields nothing, without raising"
         (attempt (shakespeare2::ollama-token-from-line "{not json")) nil)
  (check "line: valid JSON with no response field yields nothing"
         (shakespeare2::ollama-token-from-line "{\"done\":true}") nil)
  (check "line: a non-string response yields nothing"
         (shakespeare2::ollama-token-from-line "{\"response\":42}") nil))

(defun test-stream-relay-failure-e2e ()
  "An upstream that dies mid-generation reports :FAILED, not a clean ending.

   The check the port needed and did not have. ON-DONE's argument decides
   whether the handler sends EOT or a NAK, so a producer that reported :OK
   for an aborted fetch would tell the client its half-poem was the whole
   poem — silent truncation, arriving through the terminator rather than
   through the body.

   Measured: with the status mapping replaced by a constant :OK, every other
   check in this suite still passed. Nothing else looks at it."
  (format t "~%Stream: an upstream that dies mid-generation~%")
  (multiple-value-bind (tokens done)
      (%relay-tokens
       (apply #'%ollama-response-cut-short
              (%line-chunks (subseq *ollama-corpus* 0 2))))
    (check "relay: a cut-short upstream is a failure" done :failed)
    ;; Non-vacuity: this has to be the failure of a generation that started,
    ;; not one that never connected. The tokens that did arrive prove the
    ;; relay was running before the upstream went away.
    (check "relay: and the tokens that did arrive were delivered"
           tokens '("Hello" "!"))))

(defun test-stream-relay-backpressure-e2e ()
  "A generation that pauses on every chunk still finishes, which is the half
   of backpressure that can go wrong quietly.

   :PAUSE stops the upstream, and if nothing ever resumes it the fetch sits
   until *FETCH-TIMEOUT* and a healthy connection hangs. A pause is only safe
   when its waker is known. Dropping the threshold to zero makes every chunk
   pause, so every one has to be resumed — a round-trip made certain rather
   than raced against a kernel buffer.

   **What this cannot detect, stated rather than implied: the pause being
   removed altogether.** Measured — replacing the :PAUSE branch with NIL
   leaves this green, because a generation that never pauses also finishes.
   Distinguishing them needs the app to count its own pauses, which is
   instrumentation added for a test to read, and the failure it would catch
   is a performance regression rather than a wrong answer.

   The failure worth catching is the other one — pausing with no waker — and
   that this does catch: the fetch would strand, and the check would fail on
   a timeout instead of a value."
  (format t "~%Stream: backpressure pauses and resumes~%")
  (let ((shakespeare2::*pause-above-pending-bytes* 0))
    (multiple-value-bind (tokens done)
        (%relay-tokens
         (apply #'%ollama-response (%line-chunks *ollama-corpus*)))
      (check "relay: every token still arrives when every chunk pauses"
             tokens *ollama-expected-tokens*)
      (check "relay: and the generation still completes"
             done :ok))))

(defun test-stream ()
  (test-stream-token-from-line)
  (test-stream-relay-e2e)
  (test-stream-relay-failure-e2e)
  (test-stream-relay-backpressure-e2e))
