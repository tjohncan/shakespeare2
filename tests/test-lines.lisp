(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Line reassembly.
;;;
;;; MAKE-LINE-ACCUMULATOR is the one piece of app code the relay port adds,
;;; and it takes over a job the framework was doing. So the tests here are
;;; written against the framework's rules rather than against what seems
;;; reasonable: a port that quietly changed where lines end would be a
;;; behaviour change wearing a refactor's commit message.
;;;
;;; No socket, no server, no upstream — the accumulator is fed byte vectors
;;; directly, which is why it was extracted rather than written inline.
;;; ===========================================================================

(defun %feed (&rest chunks)
  "Feed CHUNKS to a fresh accumulator and return the lines it emitted.
   Each chunk is a string, encoded as it would arrive on the wire."
  (let ((out nil))
    (let ((feed (shakespeare2::make-line-accumulator
                 (lambda (line) (push line out)))))
      (dolist (c chunks)
        (funcall feed (sb-ext:string-to-octets c :external-format :utf-8))))
    (nreverse out)))

(defun test-lines-rejoins-across-chunks ()
  "A line split by a chunk boundary is delivered once, whole.

   The reason the accumulator exists. Where the split falls must not matter,
   so it is taken at every position in the string rather than at one
   convenient spot — an off-by-one in the buffer handling would survive a
   single well-chosen boundary and fail on its neighbour."
  (format t "~%Lines: rejoining across chunk boundaries~%")
  (let ((whole "{\"response\":\"hi\"}")
        (bad nil))
    (dotimes (cut (1+ (length whole)))
      (let ((got (%feed (subseq whole 0 cut)
                        (concatenate 'string (subseq whole cut) (string #\Newline)))))
        (unless (equal got (list whole))
          (push (list cut got) bad))))
    (check "lines: every split position rejoins to one line" bad nil)))

(defun test-lines-endings ()
  "LF, CRLF and bare CR each end exactly one line.

   CRLF counting once is the case with a trap in it: handled naively it
   yields a phantom empty line between every pair, which downstream would
   look like the keep-alive blanks the producer skips — a defect that hides
   inside an existing guard."
  (format t "~%Lines: line endings~%")
  (check "lines: LF ends a line"
         (%feed (format nil "a~%b~%")) '("a" "b"))
  (check "lines: CRLF ends one line, not two"
         (%feed (format nil "a~c~ab~c~a" #\Return #\Newline #\Return #\Newline))
         '("a" "b"))
  (check "lines: bare CR ends a line"
         (%feed (format nil "a~cb~c" #\Return #\Return)) '("a" "b"))
  ;; And a CRLF split *between* the CR and the LF, which is the boundary a
  ;; stateful reader gets wrong if it keeps its flag per-chunk rather than
  ;; per-stream.
  (check "lines: CRLF split across chunks still ends one line"
         (%feed (format nil "a~c" #\Return) (format nil "~ab~c~a" #\Newline #\Return #\Newline))
         '("a" "b")))

(defun test-lines-empty-and-partial ()
  "Empty lines are emitted; an unterminated tail is dropped.

   Both match the framework. Emitting empty lines leaves the skip in the
   producer's guard where it already lives. Dropping the tail is right for
   ndjson — an object no newline finished was never parseable — and is what
   the blocking path did, so emitting it here would be a new behaviour
   arriving inside a refactor."
  (format t "~%Lines: empty lines and partial tails~%")
  (check "lines: a blank line is emitted, not swallowed"
         (%feed (format nil "a~%~%b~%")) '("a" "" "b"))
  (check "lines: an unterminated tail is dropped"
         (%feed (format nil "a~%partial")) '("a"))
  (check "lines: nothing in, nothing out" (%feed "") nil))

(defun test-lines-size-cap ()
  "A line that never ends raises rather than growing without bound.

   The protection being carried over. Without it an upstream that sends no
   newline grows this buffer until the process dies — and the path this
   replaces had the cap, so losing it would trade a bounded failure for an
   unbounded one inside a commit that claims nothing changed.

   *MAX-STREAMING-LINE-SIZE* is the framework's own variable rather than a
   second one of ours, so an operator who tunes it gets both paths."
  (format t "~%Lines: the line-size cap~%")
  (let ((web-skeleton:*max-streaming-line-size* 64))
    (check "lines: an over-long line raises"
           (stringp (attempt (%feed (make-string 200 :initial-element #\x)))) t)
    (check "lines: and a line just under the cap does not"
           (%feed (concatenate 'string (make-string 32 :initial-element #\x)
                               (string #\Newline)))
           (list (make-string 32 :initial-element #\x)))))

(defun test-lines ()
  (test-lines-rejoins-across-chunks)
  (test-lines-endings)
  (test-lines-empty-and-partial)
  (test-lines-size-cap))
