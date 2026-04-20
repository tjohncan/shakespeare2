;;; Production build entry — loads the shared preamble and writes a
;;; standalone SBCL core executable. See run.lisp for the dev REPL entry.
(load (merge-pathnames "bootstrap.lisp" *load-truename*))

(sb-ext:save-lisp-and-die "shakespeare2"
  :toplevel #'shakespeare2:main
  :executable t)
