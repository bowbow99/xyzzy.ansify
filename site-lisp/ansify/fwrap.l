;;; -*- mode: lisp; package: ansify.fwrap -*-
;;;
;;; ansify/fwrap.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; References:
;;;
;;; * Allegro CL Fwrapping and Advice
;;;   [1]: http://www.franz.com/support/documentation/current/doc/fwrappers-and-advice.htm

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify.ext)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(fwrap funwrap define-fwrapper
            call-next-function
            list-fwrappers function-fwrapped-p)))

(defpackage :ansify.fwrap
  (:use :lisp)
  (:import-from :ansify.util
    #:set-indent #:define-table #:named-lambda
    #:with-gensyms)
  (:import-from :ansify.ext
    #:fwrap #:funwrap #:define-fwrapper
    #:call-next-function
    #:list-fwrappers #:function-fwrapped-p))

(in-package :ansify.fwrap)


;;;; fwrapper

(define-table fwrapper)

(defmacro define-fwrapper (name lambda-list &body body)
  "fwrapper を定義します。
fwrapper は対象となる関数が呼び出された時に、元の関数の代わりに呼び出される
関数のようなものです。通常は fwrapper 内から元の関数を呼び出すので、動作と
しては元の関数を\"包む\"ような形になります。"
  (with-gensyms ($next-fn $args $args*)
    `(progn
       (store-fwrapper
        ',name
        (named-lambda (fwrapper* ,name) (,$next-fn)
          (named-lambda (fwrapper ,name) (&rest ,$args)
            (labels ((call-next-function (&rest ,$args*)
                       (apply ,$next-fn (or ,$args* ,$args))))
              ;; TODO: arguments access via symbol-macrolet
              (block ,name ,@body)))))
       ',name)))
(set-indent 'define-fwrapper 'defun)

(defmacro with-fwrapper-properties (name-var &body body)
  `(macrolet ((primary-function () `(get ,',name-var 'primary-function))
              (active-fwrappers () `(get ,',name-var 'active-fwrappers)))
     ,@body))

(defun finalize-fwrapped-function (name)
  (with-fwrapper-properties name
    (setf (symbol-function name)
          (reduce #'funcall (mapcar #'cdr (active-fwrappers))
                  :from-end t
                  :initial-value (primary-function)))
    name))

(defun fwrap (name fwrapper &optional indicator)
  ;; TODO: setf function support
  (with-fwrapper-properties name
    (unless (primary-function)
      (setf (primary-function) (symbol-function name)))
    (setf (active-fwrappers)
          (acons indicator (find-fwrapper fwrapper)
                 (active-fwrappers)))
    (finalize-fwrapped-function name)))

(defun funwrap (name &optional indicator)
  (with-fwrapper-properties name
    (cond (indicator
           (setf (active-fwrappers)
                 (remove indicator (active-fwrappers) :key #'car))
           (finalize-fwrapped-function name))
          (t
           (when (active-fwrappers)
             (remprop name 'active-fwrappers))
           (when (primary-function)
             (setf (symbol-function name) (primary-function))
             (remprop name 'primary-function))
           name))))

(provide "ansify/fwrap")
;;; ansify/fwrap.l ends here.
