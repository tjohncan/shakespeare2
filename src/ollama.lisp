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

(defun stream-generate (user-prompt token-fn)
  "Call TOKEN-FN for each token streamed from Ollama. Blocks the worker
   thread until generation completes or the connection is dropped. TOKEN-FN
   may non-locally exit to stop generation early; the HTTP socket is closed
   by http-fetch-stream's unwind-protect.

   Ollama emits one JSON object per line. Lines that fail to parse or that
   are missing a usable \"response\" field are quietly skipped — Ollama
   sometimes sends keep-alive blanks or a final summary object."
  (http-fetch-stream :post (ollama-url)
    :headers '(("content-type" . "application/json"))
    :body (json-serialize (ollama-payload user-prompt))
    :on-line (lambda (line)
               (when (> (length line) 0)
                 (let* ((obj   (handler-case (json-parse line)
                                 (error () nil)))
                        (token (and obj (json-get obj "response"))))
                   (when (and (stringp token) (> (length token) 0))
                     (funcall token-fn token)))))))
