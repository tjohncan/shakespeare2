(in-package :shakespeare2)

;;; ===========================================================================
;;; Line reassembly over a byte stream.
;;;
;;; The framework's HTTP-FETCH-STREAM hands its caller whole lines: it owns a
;;; buffer that survives across reads, so a line split by a TCP boundary is
;;; rejoined before anyone sees it. FETCH-INTO's :ON-BODY hands over chunks
;;; instead, and chunk boundaries fall wherever the network put them — so a
;;; caller that wants lines has to keep that buffer itself.
;;;
;;; This is that buffer, extracted on its own so it can be tested without a
;;; socket, a server, or an upstream. It is the one genuinely new piece of
;;; app code the relay port needs; everything else there is rearrangement.
;;;
;;; ---------------------------------------------------------------------------
;;; Why it copies the framework's rules rather than choosing its own
;;;
;;; A port whose point is that behaviour does not change cannot afford a
;;; silently different line rule. READER-READ-BYTES ends a line on CR or on
;;; LF, treats CRLF as one ending, emits empty lines rather than skipping
;;; them, drops a trailing partial line that no newline ever terminated, and
;;; raises past *MAX-STREAMING-LINE-SIZE*. All five are reproduced here.
;;;
;;; Three of them look like details and are not:
;;;
;;;   Emitting empty lines keeps the skip where it already lives — in the
;;;   producer's own guard — instead of moving that decision in here where a
;;;   reader would have to find it.
;;;
;;;   Dropping the trailing partial is right for ndjson, where an unterminated
;;;   object was never parseable, and it is what the blocking path already
;;;   did. Emitting it would be a new behaviour introduced by a refactor.
;;;
;;;   The size cap is the one worth being loudest about. Without it an
;;;   upstream that never sends a newline grows this buffer until the process
;;;   dies, and that protection exists on the path being replaced. Losing it
;;;   in a port would be trading a bounded failure for an unbounded one, in a
;;;   commit whose message says nothing changed.
;;; ===========================================================================

(defun make-line-accumulator (line-fn)
  "Return a function of one byte vector that calls LINE-FN with each complete
   line those bytes finish.

   The returned function is stateful and single-use per stream: it holds the
   partial line between calls, which is the entire reason it exists. One
   accumulator per generation, never shared."
  (let ((buf (make-array 256 :element-type '(unsigned-byte 8)
                             :fill-pointer 0 :adjustable t))
        (prev-cr nil))
    (flet ((emit ()
             (funcall line-fn (sb-ext:octets-to-string
                               (subseq buf 0 (fill-pointer buf))
                               :external-format :utf-8))
             (setf (fill-pointer buf) 0)))
      (lambda (chunk)
        (loop for byte across chunk do
          (cond
            ((= byte 13)                ; CR ends a line
             (emit)
             (setf prev-cr t))
            ((= byte 10)                ; LF ends one, unless CR just did
             (if prev-cr
                 (setf prev-cr nil)
                 (emit)))
            (t
             (setf prev-cr nil)
             (when (>= (fill-pointer buf) *max-streaming-line-size*)
               (error "streaming response line too large (max ~d)"
                      *max-streaming-line-size*))
             (vector-push-extend byte buf))))))))
