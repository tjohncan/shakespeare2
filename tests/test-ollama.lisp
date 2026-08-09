(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Ollama request-payload tests.
;;;
;;; These pin the exact JSON text sent to /api/generate rather than checking
;;; that some object was built. The payload is a contract with a service
;;; outside this repo, so the property worth asserting is the bytes, not the
;;; shape: a change that still emits well-formed JSON but moves a key out of
;;; the object would pass a shape check and break generation outright.
;;;
;;; The expected strings below are byte-identical to what the previous
;;; web-skeleton JSON representation produced for the same inputs.
;;; ===========================================================================

(defmacro with-ollama-config ((&key (model "shakespeare") (system "be bard")
                                    temperature num-predict)
                              &body body)
  "Bind the config vars OLLAMA-PAYLOAD reads, so the expected strings below
   depend on nothing the environment did or didn't set."
  `(let ((shakespeare2::*ollama-model*       ,model)
         (shakespeare2::*system-prompt*      ,system)
         (shakespeare2::*ollama-temperature* ,temperature)
         (shakespeare2::*ollama-num-predict* ,num-predict))
     ,@body))

(defun payload-json (prompt)
  (json-serialize (shakespeare2::ollama-payload prompt)))

(defun test-ollama-payload ()
  (format t "~%Ollama: /api/generate payload~%")
  ;; Nothing optional set: no "options" key at all, rather than an empty one.
  ;; Ollama treats an absent options object and an empty one the same way,
  ;; but omitting it keeps the request identical to what shipped before.
  (with-ollama-config ()
    (check "no options"
           (payload-json "a sonnet")
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"a sonnet\",\"stream\":true}")
    (check "payload is an object, not an alist"
           (json-object-p (shakespeare2::ollama-payload "x")) t))

  ;; Both options set. OLLAMA-OPTIONS pushes temperature then num_predict,
  ;; so num_predict leads — document order is preserved through serialization.
  (with-ollama-config (:temperature 0.7d0 :num-predict 100)
    (check "both options, nested as an object"
           (payload-json "a sonnet")
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"a sonnet\",\"stream\":true,\"options\":{\"num_predict\":100,\"temperature\":0.7}}"))

  (with-ollama-config (:temperature 0.7d0)
    (check "temperature only"
           (payload-json "a sonnet")
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"a sonnet\",\"stream\":true,\"options\":{\"temperature\":0.7}}"))

  (with-ollama-config (:num-predict -1)
    (check "num_predict only, negative (Ollama's 'until context is full')"
           (payload-json "a sonnet")
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"a sonnet\",\"stream\":true,\"options\":{\"num_predict\":-1}}"))

  ;; The prompt is user input and reaches Ollama inside a JSON string. Quotes
  ;; and newlines are the two characters a browser can trivially put in the
  ;; box that would break the document if they were not escaped.
  (with-ollama-config ()
    (check "prompt with quotes and newline is escaped"
           (payload-json (format nil "say \"hi\"~%twice"))
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"say \\\"hi\\\"\\ntwice\",\"stream\":true}"))

  ;; The system prompt comes from SPIRIT.md, which is UTF-8 and hand-edited.
  (with-ollama-config (:system "thou art—naught")
    (check "non-ASCII system prompt survives"
           (payload-json "x")
           "{\"model\":\"shakespeare\",\"system\":\"thou art—naught\",\"prompt\":\"x\",\"stream\":true}")))

(defun test-ollama-options ()
  (format t "~%Ollama: options assembly~%")
  (with-ollama-config ()
    (check "nothing set → no options at all"
           (shakespeare2::ollama-options) nil))
  (with-ollama-config (:temperature 0d0)
    ;; 0.0 is a legitimate temperature (fully deterministic) and must not be
    ;; dropped the way a NIL is. WHEN on a float is true for 0d0, so this is
    ;; testing that the guard stayed a presence check and never became a
    ;; truthiness check on the value.
    (check "temperature 0.0 is kept"
           (payload-json "x")
           "{\"model\":\"shakespeare\",\"system\":\"be bard\",\"prompt\":\"x\",\"stream\":true,\"options\":{\"temperature\":0.0}}")))

(defun test-ollama ()
  (test-ollama-payload)
  (test-ollama-options))
