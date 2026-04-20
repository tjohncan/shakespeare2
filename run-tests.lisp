;;; Dev entry — loads the shared preamble (which sets up ASDF + web-skeleton
;;; and conditionally loads web-skeleton-tls), then loads shakespeare2-tests
;;; and runs the suite. Exits non-zero on failure so CI notices.
(load (merge-pathnames "bootstrap.lisp" *load-truename*))
(asdf:load-system "shakespeare2-tests")
(unless (shakespeare2-tests:test)
  (sb-ext:exit :code 1))
