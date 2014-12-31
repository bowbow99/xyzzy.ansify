;;; -*- mode: lisp; package: user -*-
;;;
;;; ansify/tool/build.l

;;; Code:

(in-package :user)

(defparameter *ansify-root* (merge-pathnames ".." (directory-namestring *load-pathname*)))

(defun remove-ansify ()
  "ansify 関連のものを削除する。"
  (interactive)
  ;; Package
  (dolist (pkg (remove-if-not (lambda (pkg)
                                (string-match "^ansify" (package-name pkg)))
                              (list-all-packages)))
    (dolist (using (package-used-by-list pkg))
      (unuse-package pkg using))
    (delete-package pkg))
  ;; Module info
  (setf *modules* (delete-if (lambda (m) (string-match "^ansify" m)) *modules*))
  t)

(defun byte-compile-ansify ()
  "ansify をコンパイルする"
  (interactive)
  ;; Delete all fasls
  (directory (merge-pathnames "site-lisp" *ansify-root*) :wild "*.lc"
             :recursive t :absolute t
             :callback #'delete-file)
  ;; Compile all sources
  (labels ((msg (fmt &rest args)
             (if (interactive-p)
               (apply #'message fmt args)
               (apply #'format t (concat fmt "~%") args))))
    (dolist (src (directory (merge-pathnames "site-lisp" *ansify-root*) :wild "*.l"
                            :recursive t :absolute t))
      (remove-ansify)
      (msg ";;; Compiling ~A..." src)
      (compile-file src)
      (msg ";;; Compiling ~A...done" src))
    t))

;; ansify/tool/build.l ends here.
