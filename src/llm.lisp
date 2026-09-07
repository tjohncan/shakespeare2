(in-package :shakespeare2)

;;; ===========================================================================
;;; The generator seam.
;;;
;;; One configured service produces tokens; the handler consumes them and
;;; knows nothing about which one ran. STREAM-GENERATE is the seam and the
;;; only entry point the rest of the app uses.
;;;
;;; ---------------------------------------------------------------------------
;;; Why the seam is shaped "produce tokens into this connection" rather than
;;; "which URL do I fetch"
;;;
;;; :NONE performs no outbound request at all — no host, no port, no upstream,
;;; nothing to connect to. A seam cut at the fetch would have no room for it,
;;; and :NONE would have to bypass the seam rather than be an instance of it,
;;; which would leave the seam validated by exactly one implementation.
;;;
;;; That constraint is worth keeping when the third service arrives: the seam
;;; must not assume a fetch exists, and must not assume the producer is
;;; synchronous. Both assumptions are cheap to make and expensive to unmake.
;;;
;;; ---------------------------------------------------------------------------
;;; Why an ECASE and not a registry
;;;
;;; Three values, known at compile time, all in this file. A protocol class, a
;;; hook table, or a registry would be building for a caller that does not
;;; exist — and this project has spent a campaign learning what unmotivated
;;; capability costs. The framework's own *DNS-LOOKUP-FN* is the precedent for
;;; a seam that stayed a variable.
;;; ===========================================================================

(defparameter *disabled-message*
  "FUNCTIONALITY DISABLED

the bard has been powered down;
his cloud demanded gold.
this page remains a keepsake --
the poems have been told."
  "Streamed in place of a poem when LLM_SERVICE is none. Kept as a working
   page rather than an error: the deployment that needs this is one where the
   model costs money nobody is spending, and a visitor should meet a finished
   thing rather than a broken one.")

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab)
      (char= c #\Newline) (char= c #\Return)))

(defun split-into-tokens (text)
  "TEXT as a list of tokens, each a run of non-whitespace plus the whitespace
   that follows it. Concatenating the result reproduces TEXT exactly, which is
   the property that makes this safe to stream: the client reassembles by
   appending, so any token boundary is legal but no character may be invented
   or dropped."
  (let ((out nil)
        (i 0)
        (n (length text)))
    (loop while (< i n) do
      (let ((start i))
        (loop while (and (< i n) (not (%whitespace-p (char text i)))) do (incf i))
        (loop while (and (< i n) (%whitespace-p (char text i))) do (incf i))
        (push (subseq text start i) out)))
    (nreverse out)))

(defun none-generate (user-prompt token-fn)
  "Stream *DISABLED-MESSAGE* through TOKEN-FN, one token at a time.

   Token by token rather than in one frame, for two reasons. The client's
   typewriter animation is driven by arrival, so a single frame renders as a
   paste and the page looks stalled rather than finished. And it makes this a
   real implementation of the seam instead of a shortcut around it — a
   producer that emitted its whole output at once would leave the seam
   validated by one shape.

   No artificial delay between tokens. A sleep here would block the worker for
   the length of the animation, which is precisely the ceiling the relay work
   exists to remove; the client can animate arrival on its own clock.

   USER-PROMPT is ignored, deliberately and visibly: this service produces the
   same answer for every prompt, and pretending otherwise would be the kind of
   shape a later reader has to disprove.

   Answers T if TOKEN-FN asked it to stop, NIL if it ran to the end. TOKEN-FN
   returning :STOP means the caller has stopped wanting output, and a producer
   that kept producing anyway would be the defect this seam exists to make
   visible — on this backend the cost is a few conses, on :OLLAMA it is a
   model that keeps generating."
  (declare (ignore user-prompt))
  (dolist (token (split-into-tokens *disabled-message*) nil)
    (when (eq (funcall token-fn token) :stop)
      (return t))))

(defparameter *pause-above-pending-bytes* (* 64 1024)
  "Backlog on the target connection, in bytes, above which the upstream is
   paused.

   Well under the framework's *MAX-WRITE-BACKLOG*, which is the hard cap that
   closes a connection, and far above ordinary token traffic — a browser
   keeping up sits at zero. The gap between the two is deliberate: pausing
   near the cap would mean the first pause and the connection's death arrive
   together, which is no backpressure at all.")

(defun ollama-start (conn user-prompt on-token on-done)
  "The :OLLAMA arm. Starts an outbound fetch against CONN and returns as soon
   as the request is away; tokens and the ending arrive later, from the event
   loop.

   This is what the seam was reshaped for. The worker is not held for the
   length of a generation, so concurrent poems stop being bounded by worker
   count — which on the micro profile, at half a CPU, was a ceiling of one.

   Three things this has to do that the blocking version got for free.

   Lines. :ON-BODY delivers chunks, not lines, so MAKE-LINE-ACCUMULATOR keeps
   the partial across chunk boundaries — one accumulator per generation,
   captured here, never shared.

   Backpressure. Returning :PAUSE stops reading the upstream when the target
   is falling behind. The framework resumes on its own when the backlog
   drains, because a detached fetch records its target and every state that
   can be relayed into reaches the drain path — so this is a pause with a
   known waker, which is the only kind that is safe to take.

   Endings. FETCH-INTO signals rather than returning NIL when it refuses, and
   a signalling call has done nothing, so :THEN will never run — ON-DONE has
   to be called here instead, or the handler never sends a terminator and the
   page hangs with an open output box.

   A non-NIL status is success even when it is 500, which preserves what the
   blocking path did: HTTP-FETCH-STREAM returned the status and this producer
   never looked at it, so an Ollama error page has always arrived as an empty
   poem rather than a failure. NIL is the abort sentinel — a stop we asked for
   or a failure we did not, and STOPPED is what tells the two apart.

   Stopping. ON-TOKEN returning :STOP is relayed to the framework as :ON-BODY
   returning :STOP, which closes the outbound and leaves CONN alone. The flag
   is set once and never cleared, mirroring the framework's own rule for the
   verdict: a stop describes the generation rather than the moment, and no
   later token retracts it. It is read in two places for two different
   questions — :ON-BODY asks whether to end the fetch, :THEN asks what to call
   the ending."
  (let* ((stopped nil)
         (feed (make-line-accumulator
                (lambda (line)
                  (let ((token (ollama-token-from-line line)))
                    (when token
                      (when (eq (funcall on-token token) :stop)
                        (setf stopped t))))))))
    (handler-case
        (fetch-into
         conn
         (http-fetch
          :post (ollama-url)
          :headers '(("content-type" . "application/json"))
          :body (json-serialize (ollama-payload user-prompt))
          :on-body
          (lambda (out chunk)
            (declare (ignore out))
            (funcall feed chunk)
            ;; Stop outranks pause, and not only because the framework tests
            ;; them in that order. Pausing a fetch this callback has just
            ;; asked to end would be asking the upstream to wait for output
            ;; nobody is going to read.
            (cond
              (stopped :stop)
              ((> (connection-write-pending conn) *pause-above-pending-bytes*)
               :pause)))
          :then
          (lambda (status headers body)
            (declare (ignore headers body))
            (funcall on-done (cond (stopped :stopped)
                                   (status  :ok)
                                   (t       :failed)))
            nil)))
      (error (e)
        ;; The refusal path: no fetch was started, so nothing else will ever
        ;; call ON-DONE.
        (log-error "ollama fetch-into refused: ~a" e)
        (funcall on-done :failed)))))

(defun none-start (conn user-prompt on-token on-done)
  "The :NONE arm. Produces its whole output and finishes before returning.

   Reports :STOPPED rather than :OK when ON-TOKEN cut it short, for the same
   reason the :OLLAMA arm does: the caller asked for the ending, and telling
   it the generation completed would be a small lie in the one place the app
   decides whether to send a terminator or an error."
  (declare (ignore conn))
  (funcall on-done (if (none-generate user-prompt on-token) :stopped :ok)))

(defun start-generation (conn user-prompt &key on-token on-done)
  "Start producing tokens for USER-PROMPT into CONN's conversation.

   ON-TOKEN is called with each token, and may return :STOP to end the
   generation early. ON-DONE is called exactly once, with :OK, :STOPPED or
   :FAILED, when there will be no more.

   :STOPPED is a separate answer from :FAILED on purpose. Both arrive after a
   generation that did not run to the end, and only one of them is something
   going wrong — a caller that had to tell them apart by remembering what it
   asked for would be re-deriving a fact the producer already knows. On
   :OLLAMA the two are genuinely indistinguishable downstream: the framework
   delivers the same abort sentinel either way, and the flag set on the way
   into the stop is the only thing that separates them.

   **Returning does not mean finished.** :NONE calls ON-DONE before it
   returns and :OLLAMA does so too while it is still blocking, but neither is
   a promise: the relay port makes :OLLAMA return as soon as the request is
   away, with tokens and completion arriving later from the event loop. A
   caller that treats the return of this function as the end of generation
   works today and breaks then, silently, by sending its terminator before
   the poem.

   The non-local exit ON-TOKEN used to take is gone, and :STOP is what
   replaced it. That exit worked only while the producer was synchronous: it
   unwound out of the blocking HTTP read, whose UNWIND-PROTECT closed the
   socket and stopped the model. After the port there is no stack to unwind —
   ON-TOKEN runs from the event loop — so the same instinct would silently do
   nothing to the upstream. A returned verdict travels instead of unwinding,
   which is the shape that survives an asynchronous producer.

   *LLM-SERVICE* is validated at startup by PARSE-LLM-SERVICE, so the ECASE
   below cannot be reached with an unknown value by way of configuration —
   only by code setting the variable directly, which is a bug worth the
   raise."
  (ecase *llm-service*
    (:ollama (ollama-start conn user-prompt on-token on-done))
    (:none   (none-start   conn user-prompt on-token on-done))))
