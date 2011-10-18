;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/function.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/config")
  (require "ansify/condition")
  (require "ansify/restart"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(defun
                      fdefinition
                      function-lambda-expression
                      )))

(lisp:defun get-setf-function-symbol (name)
  (unless (and (consp name)
               (eql (first name) 'setf)
               (symbolp (second name))
               (null (cddr name)))
    (error "ïsê≥Ç»ä÷êîñºÇ≈Ç∑: ~S" name))
  (let ((accessor (second name)))
    (intern (format nil "(setf ~A)" accessor)
            (symbol-package accessor))))

(lisp:defun canonicalize-function-name (name)
  (if (symbolp name) name
    (get-setf-function-symbol name)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro defun (name (&rest lambda-list) &body body)
    (cond ((symbolp name)
           `(progn
              (when (and (fboundp ',name)
                         *warn-on-redefine-function*)
                (warn 'redefining-function :name ',name :type 'function))
              (lisp:defun ,name ,lambda-list ,@body)))
          ((consp name)
           (let ((sym (get-setf-function-symbol name)))
             `(progn
                (when (and (fboundp ',sym)
                           *warn-on-redefine-function*)
                  (warn 'redefining-function :name ',name :type 'function))
                (lisp:defun ,sym ,lambda-list ,@body)
                (defsetf ,(second name) ,(cdr lambda-list) (,(car lambda-list))
                  (list ',sym ,@lambda-list))))))))

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm

(lisp:defun fdefinition (function-name)
  (symbol-function (canonicalize-typespec function-name)))

(defsetf fdefinition (function-name) (new-definition)
  `(setf (symbol-function ,(canonicalize-function-name function-name))
         ,new-definition))


(provide "ansify/function")
;;; ansify/function.l ends here.
