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

(defun stream-generate (user-prompt token-fn)
  "Call TOKEN-FN for each token streamed from Ollama. Blocks the worker
   thread until generation completes or the connection is dropped. TOKEN-FN
   may non-locally exit to stop generation early; the HTTP socket is closed
   by http-fetch-stream's unwind-protect.

   Ollama emits one JSON object per line. Lines that fail to parse or that
   are missing a usable \"response\" field are quietly skipped — Ollama
   sometimes sends keep-alive blanks or a final summary object."
  (let* ((opts    (ollama-options))
         (payload `(("model"  . ,*ollama-model*)
                    ("system" . ,*system-prompt*)
                    ("prompt" . ,user-prompt)
                    ("stream" . t)
                    ,@(when opts `(("options" . ,opts))))))
    (http-fetch-stream :post (ollama-url)
      :headers '(("content-type" . "application/json"))
      :body (json-serialize payload)
      :on-line (lambda (line)
                 (when (> (length line) 0)
                   (let* ((obj   (handler-case (json-parse line)
                                   (error () nil)))
                          (token (and obj (json-get obj "response"))))
                     (when (and (stringp token) (> (length token) 0))
                       (funcall token-fn token))))))))
