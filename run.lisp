;;; Dev REPL entry — loads the shared preamble and starts the server in this
;;; process. See build.lisp for the save-lisp-and-die production entry.
(load (merge-pathnames "bootstrap.lisp" *load-truename*))
(shakespeare2:start)
