;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/compile.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

;; NOTE: just put "compiler" package here to allow other libraries to use
;; `compiler::optimize-form`.

(defpackage :compiler
  (:use :lisp :editor)
  (:internal-size 200)
  (:external-size 10))

(provide "ansify/compile")
;;; ansify/compile.l ends here.
