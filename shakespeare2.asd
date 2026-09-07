(defsystem "shakespeare2"
  :description "Shakespeare-style poem generator — WebSocket streaming from a configurable LLM service"
  :version "0.2.0"
  :depends-on ("web-skeleton")
  :serial t
  :components ((:file "src/package")
               (:file "src/config")
               (:file "src/lines")
               (:file "src/ollama")
               (:file "src/llm")
               #+shakespeare2/auth (:file "src/auth")
               #+shakespeare2/auth (:file "src/admin")
               (:file "src/handler")))
