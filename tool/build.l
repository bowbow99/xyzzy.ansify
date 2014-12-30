;;; -*- mode: lisp; package: user -*-
;;;
;;; ansify/tool/build.l

;;; Code:

(in-package :user)


;;;; Clean up existing (possibly older version of) ansify stuff from xyzzy.

;;;;; Delete ansify packages
(dolist (pkg '(:ansify :ansify.ext))
  (when (find-package pkg)
    (mapc (lambda (using)
            (unuse-package pkg using))
          (package-used-by-list pkg))
    (delete-package pkg)))

;;;;; Clear module info
(setf *modules* (delete-if (lambda (m) (string-match "^ansify" m)) *modules*))


;;;; Compile all the files

(directory (merge-pathnames "../site-lisp/ansify"
                            (directory-namestring *load-pathname*))
           :wild "*.l" :absolute t :recursive t
           :callback #'compile-file)

;; ansify/tool/build.l ends here.
