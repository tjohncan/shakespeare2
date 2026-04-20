(require :asdf)

;;; Shared preamble for run.lisp (dev REPL entry) and build.lisp
;;; (save-lisp-and-die entry). Kept in one place so the ASDF registry setup
;;; and the TLS bootstrap can't drift between the two.
;;;
;;; Loaded by both entry scripts via (load "bootstrap.lisp") relative to
;;; their own *load-truename*. Must not call START or SAVE-LISP-AND-DIE —
;;; the caller is responsible for whatever comes after the system is loaded.

;;; Auth build flag — set SHAKESPEARE2_AUTH=true to compile auth support in.
(when (string= (sb-ext:posix-getenv "SHAKESPEARE2_AUTH") "true")
  (pushnew :shakespeare2/auth *features*))

;;; Register web-skeleton and shakespeare2 with ASDF.
;;; Override WEB_SKELETON_PATH to point at a non-sibling checkout.
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)

(push (let ((env (sb-ext:posix-getenv "WEB_SKELETON_PATH")))
        (if env
            (uiop:ensure-directory-pathname env)
            (merge-pathnames "../web-skeleton/"
                             (make-pathname :directory (pathname-directory *load-truename*)))))
      asdf:*central-registry*)

;;; Outbound HTTPS (to the auth server) needs libssl. Non-auth builds
;;; never make outbound HTTPS calls, so skip TLS entirely — the pure-Lisp
;;; SHA-256 in web-skeleton handles static-file ETags without libssl.
#+shakespeare2/auth
(handler-case (asdf:load-system "web-skeleton-tls")
  (error ()
    (error "TLS is required when SHAKESPEARE2_AUTH=true (install libssl)")))

(asdf:load-system "shakespeare2")
