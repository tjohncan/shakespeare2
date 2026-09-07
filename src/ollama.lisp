(in-package :shakespeare2)

;;; ===========================================================================
;;; Ollama streaming client.
;;;
;;; Streams token-by-token responses from Ollama's /api/generate endpoint
;;; using the framework's http-fetch-stream primitive (blocking line reader).
;;; ===========================================================================

(defun ollama-url ()
  (format nil "http://~a:~d/api/generate" *ollama-host* *ollama-port*))

(defun ollama-options ()
  "Build the Ollama options object from config, omitting unset fields."
  (let ((opts '()))
    (when *ollama-temperature*
      (push (cons "temperature" *ollama-temperature*) opts))
    (when *ollama-num-predict*
      (push (cons "num_predict" *ollama-num-predict*) opts))
    opts))

(defun ollama-payload (user-prompt)
  "Build the /api/generate request object.

   Split out of STREAM-GENERATE so the exact bytes Ollama receives can be
   asserted without a running model. Inline, the only way to exercise this
   construction was a real generation call, which put the app's whole
   purpose out of reach of the test suite.

   Both levels are objects. MAKE-JSON-OBJECT does not recurse, so the
   nested options value is wrapped in its own right; left bare it would
   raise at serialize time, since a dotted pair is not a valid array
   element."
  (let ((opts (ollama-options)))
    (make-json-object
     `(("model"  . ,*ollama-model*)
       ("system" . ,*system-prompt*)
       ("prompt" . ,user-prompt)
       ("stream" . t)
       ,@(when opts `(("options" . ,(make-json-object opts))))))))

(defun ollama-token-from-line (line)
  "The token LINE carries, or NIL if it carries none.

   Pure, and separated from the transport on purpose. What Ollama's lines
   mean does not change when the way they arrive does — so this keeps its
   own tests across the move from a blocking line reader to a chunked one,
   and those tests go on detecting a parsing regression while the plumbing
   underneath is replaced.

   A line is skipped three ways: it fails to parse, its \"response\" is
   absent or not a string, or its \"response\" is the empty string.

   The last is the case that actually arises. The final summary object
   carries \"response\":\"\" beside \"done\":true — the field is present and
   empty, not missing. TEST-STREAM's corpus is a captured response and shows
   it. An earlier version of this docstring said \"missing a usable response
   field\", which is close enough to sound right and wrong enough that a
   fixture written from it asserted a shape Ollama never sends.

   The empty-line guard is defence rather than a described phenomenon. No
   blank line appeared in the capture, and one capture cannot prove they
   never occur — so the guard stays and the claim that Ollama sends them
   does not."
  (when (> (length line) 0)
    (let* ((obj   (handler-case (json-parse line)
                    (error () nil)))
           (token (and obj (json-get obj "response"))))
      (when (and (stringp token) (> (length token) 0))
        token))))

;;; OLLAMA-GENERATE, the blocking producer, lived here and is gone. It called
;;; HTTP-FETCH-STREAM and held the worker for the length of a generation;
;;; OLLAMA-START in llm.lisp does the same work through FETCH-INTO and holds
;;; nothing. Deleted rather than kept as a second path: it had no caller left,
;;; and a function alive only because tests still call it is a test asserting
;;; that the code it tests exists.
;;;
;;; What was worth keeping from it is above. OLLAMA-TOKEN-FROM-LINE is the
;;; half that survived the transport change untouched, which is why it was
;;; split out before the change rather than after.
