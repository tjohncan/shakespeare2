(in-package :shakespeare2-tests)

;;; ===========================================================================
;;; Config parsers — pure functions from src/config.lisp. No env vars, no
;;; side effects. These lock in the input validation that load-config
;;; depends on at startup so a typo in a parser can't ship quietly.
;;; ===========================================================================

(defun test-config-parse-ipv4 ()
  (format t "~%Config: parse-ipv4~%")
  (check "loopback"
         (coerce (shakespeare2::parse-ipv4 "127.0.0.1") 'list)
         '(127 0 0 1))
  (check "all-zeros"
         (coerce (shakespeare2::parse-ipv4 "0.0.0.0") 'list)
         '(0 0 0 0))
  (check "broadcast"
         (coerce (shakespeare2::parse-ipv4 "255.255.255.255") 'list)
         '(255 255 255 255))
  (check-error "too few octets" (shakespeare2::parse-ipv4 "127.0.0"))
  (check-error "too many octets" (shakespeare2::parse-ipv4 "1.2.3.4.5"))
  (check-error "octet out of range" (shakespeare2::parse-ipv4 "127.0.0.256"))
  (check-error "non-numeric" (shakespeare2::parse-ipv4 "localhost"))
  (check-error "empty" (shakespeare2::parse-ipv4 ""))
  ;; Framework's parse-ipv4-literal rejects leading zeros (except the bare
  ;; '0') to keep dotted-quad strictly canonical — locks the new behavior.
  (check-error "leading zeros rejected" (shakespeare2::parse-ipv4 "127.0.0.01")))

(defun test-config-split-csv ()
  (format t "~%Config: split-csv~%")
  (check "nil input" (shakespeare2::split-csv nil) nil)
  (check "empty string" (shakespeare2::split-csv "") nil)
  (check "single value" (shakespeare2::split-csv "alpha") '("alpha"))
  (check "multiple values"
         (shakespeare2::split-csv "alpha,beta,gamma")
         '("alpha" "beta" "gamma"))
  (check "whitespace trimmed"
         (shakespeare2::split-csv "  alpha , beta  ,gamma ")
         '("alpha" "beta" "gamma"))
  (check "empty elements dropped"
         (shakespeare2::split-csv "alpha,,beta, ,gamma")
         '("alpha" "beta" "gamma"))
  (check "trailing comma"
         (shakespeare2::split-csv "alpha,beta,")
         '("alpha" "beta")))

(defun test-config-parse-nonneg-float ()
  (format t "~%Config: parse-nonneg-float~%")
  (check "integer"       (shakespeare2::parse-nonneg-float "42") 42d0)
  (check "decimal"       (shakespeare2::parse-nonneg-float "0.5") 0.5d0)
  (check "zero"          (shakespeare2::parse-nonneg-float "0") 0d0)
  (check "zero-decimal"  (shakespeare2::parse-nonneg-float "0.0") 0d0)
  (check "leading dot"   (shakespeare2::parse-nonneg-float ".75") 0.75d0)
  (check "trailing dot"  (shakespeare2::parse-nonneg-float "3.") 3d0)
  ;; Reject untrusted-input hazards: sign, exponent, whitespace, garbage.
  (check "reject negative"   (shakespeare2::parse-nonneg-float "-1") nil)
  (check "reject positive-sign" (shakespeare2::parse-nonneg-float "+1") nil)
  (check "reject exponent"   (shakespeare2::parse-nonneg-float "1e5") nil)
  (check "reject leading ws" (shakespeare2::parse-nonneg-float " 1") nil)
  (check "reject double dot" (shakespeare2::parse-nonneg-float "1.2.3") nil)
  (check "reject letters"    (shakespeare2::parse-nonneg-float "fubar") nil)
  (check "empty"             (shakespeare2::parse-nonneg-float "") nil)
  (check "nil"               (shakespeare2::parse-nonneg-float nil) nil))

(defun test-config ()
  (test-config-parse-ipv4)
  (test-config-split-csv)
  (test-config-parse-nonneg-float))
