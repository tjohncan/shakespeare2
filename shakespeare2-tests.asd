(defsystem "shakespeare2-tests"
  :description "Regression suite for shakespeare2"
  :version "0.2.0"
  :depends-on ("shakespeare2" "web-skeleton-test-harness")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/run")
               (:file "tests/test-config")
               (:file "tests/test-ollama")
               (:file "tests/test-handler")
               #+shakespeare2/auth (:file "tests/test-auth")
               #+shakespeare2/auth (:file "tests/test-admin")))
