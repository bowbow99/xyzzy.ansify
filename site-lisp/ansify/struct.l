;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/struct.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; NOTE: currently, this is just a quick workaround
;;; to work on closette. I'd reimplement it properly later.

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/destructuring-bind"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:defstruct)))


;;;; undefined `:print-function` fix

(defun defstruct-quote-print-function (expn)
  (destructuring-bind (eval-when situations
                        (let ((var (si:*make-structure-definition
                                    name type include constructors
                                    print-function named slots)))
                          . body))
      expn
    (let ((print-function* (when (and (consp print-function)
                                      (eq (car print-function) 'function))
                             (list 'quote (cadr print-function)))))
    `(eval-when ,situations
       (let ((,var (si:*make-structure-definition
                    ,name ,type ,include ,constructors
                    ,print-function* ,named ,slots)))
         ,@body)))))


;;;; put all together

(defun expand-defstruct (env form)
  (let ((expn (macroexpand `(lisp:defstruct ,@(cdr form)) env)))
    (setf expn (defstruct-quote-print-function expn))
    expn))

(defmacro defstruct (&environment env &whole form
                     name-and-options &rest slot-descriptions)
  (expand-defstruct env form))


(provide "ansify/struct")
;;; ansify/struct.l ends here.
