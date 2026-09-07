(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Test harness utilities
;;;
;;; Minimal check / check-error with a pass/fail counter. Intentionally
;;; tiny — the goal is to get a regression suite in place, not to build a
;;; testing framework. Matches the shape of web-skeleton's own run.lisp
;;; so a reader familiar with the framework's tests reads these ones
;;; without context-switching.
;;; ===========================================================================

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)
(defvar *failed-names* nil)

(defun check (name actual expected &key (test #'equal))
  (if (funcall test actual expected)
      (incf *tests-passed*)
      (progn
        (incf *tests-failed*)
        (push name *failed-names*)
        (format t "  FAIL ~a~%    expected: ~s~%    actual:   ~s~%"
                name expected actual))))

(defmacro check-error (name form)
  `(handler-case
       (progn ,form
              (incf *tests-failed*)
              (push ,name *failed-names*)
              (format t "  FAIL ~a (no error signaled)~%" ,name))
     (error () (incf *tests-passed*))))

(defmacro attempt (&body body)
  "Evaluate BODY, answering its value, or the error text if it raised.

   For a CHECK whose subject can raise — anything that reaches a socket or
   parses input it did not write. An uncaught raise ends the run mid-file:
   no failure list, no totals, and every later assertion unexecuted, which
   makes the check unreadable under the discipline these tests are read
   with. A raise that becomes a failed CHECK carrying the condition text
   costs nothing and stays countable.

   Taken from web-skeleton's run.lisp, which grew it for the same reason.
   Measured here: without it, removing STREAM-GENERATE's HANDLER-CASE made
   the suite exit 1 with no FAIL line at all — the malformed-line raise
   killed the run rather than reporting it."
  `(handler-case (progn ,@body)
     (error (e) (princ-to-string e))))

(defun report-suite (label)
  (format t "~%=== ~a ===~%  passed: ~d~%  failed: ~d~%"
          label *tests-passed* *tests-failed*)
  (when *failed-names*
    (format t "  failed names:~%")
    (dolist (n (reverse *failed-names*))
      (format t "    ~a~%" n))))

(defun test ()
  "Run every test suite. Exits non-zero via the caller if any checks fail."
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== shakespeare2 tests ===~%")
  (test-config)
  (test-lines)
  (test-ollama)
  (test-stream)
  (test-llm)
  (test-handler)
  #+shakespeare2/auth (test-auth)
  #+shakespeare2/auth (test-admin)
  (report-suite "shakespeare2 total")
  (zerop *tests-failed*))
