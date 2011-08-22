;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/setf.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(setf (symbol-function 'get-setf-expansion) #'get-setf-method-multiple-value
      (symbol-function 'define-setf-expander) #'define-setf-method)

(provide "ansify/setf")
;;; ansify/setf.l ends here.
