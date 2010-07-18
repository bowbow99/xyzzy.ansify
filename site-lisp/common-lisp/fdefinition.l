;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/fdefinition.l
;;;
;;; [CLHS: Issue FUNCTION-NAME Writeup]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Issues/iss174_w.htm
;;;

(require "common-lisp/package")
(in-package :common-lisp)

(export '(fdefinition))

(defun fdefinition (specifier)
  (symbol-function specifier)
  )


;;; common-lisp/fdefinition.l ends here.
