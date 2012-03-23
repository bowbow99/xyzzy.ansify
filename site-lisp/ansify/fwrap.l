;;; -*- mode: lisp; package: ansify.fwrap -*-
;;;
;;; ansify/fwrap.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify.ext)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(fwrap funwrap define-fwrapper
            list-fwrappers function-fwrapped-p)))

(defpackage :ansify.fwrap
  (:use :lisp)
  (:import-from :ansify.util
    #:set-indent)
  (:import-from :ansify.ext
    #:fwrap #:funwrap #:define-fwrapper
    #:list-fwrappers #:function-fwrapped-p))

(in-package :ansify.fwrap)


;;;; fwrapper

(defstruct fwrapper
  name
  lambda-list
  body)

(defparameter *fwrappers* nil)

(defun save-fwrapper (fwrapper)
  (let ((others (remove (fwrapper-name fwrapper) *fwrappers*
                        :key #'fwrapper-name :test #'equal)))
    (setf *fwrappers* (cons fwrapper others))))

(defun find-fwrapper (name)
  (find name *fwrappers* :key #'fwrapper-name :test #'equal))

(defmacro define-fwrapper (name lambda-list &body body)
  `(save-fwrapper (make-fwrapper :name ',name
                                 :lambda-list ',lambda-list
                                 :body ',body)))
(set-indent 'define-fwrapper 'defun)




#| reference: encap.l

(defun encapsulate (symbol type body)
  (let ((new-dholder (gensym))
        (new-pholder (gensym))
        (old-def (symbol-function symbol)))
    (setf (symbol-function 'encap-type) type
          (get new-pholder 'encap-type) type
          (get new-pholder 'encap-next) new-dholder)
    (encap-set-pholder new-dholder (encap-get-pholder symbol))
    (encap-set-pholder symbol new-pholder)
    (unless (consp (car body))
      (setq body (list body)))
    (setf (symbol-function symbol)
          (lambda (&rest argument-list
                   &aux (basic-definition ,#'(lambda (&rest args)
                                               (apply (symbol-function new-dholder) args))))
            ,@body))
    symbol))

(defun encap-type (dholder)
  (get (encap-get-pholder dholder) 'encap-type))

(defun encap-next (dholder)
  (get (encap-get-pholder dholder) 'encap-next))

(defun encap-get-pholder (dholder)
  (gethash dholder *encap-hash-table*))

(defun encap-set-pholder (dholder pholder)
  (setf (gethash dholder *encap-hash-table*) pholder))

(defun unencapsulate (symbol type)
  (do ((dholder symbol (encap-next dholder)))
      ((null (encap-get-pholder dholder)) nil)
    (if (eq type (encap-type dholder))
      (let ((next-dholder (encap-next dholder)))
        (setf (symbol-function dholder)
              (symbol-function next-dholder))
        (encap-set-pholder dholder (encap-get-pholder next-dholder))
        (remhash next-dholder *encap-hash-table*)
        (return-from unencapsulate t)))))

(defun encapsulated-p (symbol type)
  (do ((dholder symbol (encap-next dholder)))
      ((null (encap-get-pholder dholder)) nil)
    (if (eq type (encap-type dholder))
      (return-from encapsulated-p t))))
|#

(provide "ansify/fwrap")
;;; ansify/fwrap.l ends here.
