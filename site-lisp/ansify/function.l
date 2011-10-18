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
    (error "不正な関数名です: ~S" name))
  (let ((accessor (second name)))
    (intern (format nil "(setf ~A)" accessor)
            (symbol-package accessor))))

(lisp:defun canonicalize-function-name (name)
  (if (symbolp name) name
    (get-setf-function-symbol name)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro defun (name (&rest lambda-list) &body body)
    (labels ((warn-redefinition (sym name)
               `(when (and (fboundp ',sym)
                           *warn-on-redefine-function*)
                  (warn 'redefining-function :name ',name :type 'function))))
    (cond ((symbolp name)
           `(progn
              ,(warn-redefinition name name)
              (lisp:defun ,name ,lambda-list ,@body)))
          ((consp name)
           (let ((sym (get-setf-function-symbol name)))
             `(progn
                ,(warn-redefinition sym name)
                (lisp:defun ,sym ,lambda-list ,@body)
                (defsetf ,(second name) ,(cdr lambda-list) (,(car lambda-list))
                  (list ',sym ,@lambda-list)))))))))

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm

(lisp:defun fdefinition (function-name)
  (symbol-function (canonicalize-typespec function-name)))

(defsetf fdefinition (function-name) (new-definition)
  (let ((name (canonicalize-function-name function-name)))
    (when (special-form-p name)
      (error 'simple-program-error
             :format-string "スペシャルオペレータの fdefinition は変更できません: ~S"
             :format-arguments (list name))))
  `(if (or (functionp ,new-definition)
           (si:*builtin-function-p ,new-definition))
     (setf (symbol-function ,name) ,new-definition)
     (error 'type-error :datum ,new-definition :expected-type 'function)))


(provide "ansify/function")
;;; ansify/function.l ends here.
