;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/defun.l
;;;
;;; - add support for (setf ...) as function name
;;;

(require "common-lisp/package")
(require "common-lisp/setf-function-name")

(in-package :common-lisp)

(shadowing-export '(defun))

(defmacro defun (name (&rest lambda-list) &body body)
  (if (%setf-function-name-p name)
      `(progn
         (lisp:defun ,(%function-symbol name) ,lambda-list ,@body)
         ',name)
    `(lisp:defun ,name ,lambda-list ,@body)))

;;; common-lisp/defun.l ends here.
