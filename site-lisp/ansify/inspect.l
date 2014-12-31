;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/inspect.l
;;;
;;; Author:     bowbow99 <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:


;;;; Package

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/gfm"))

(in-package :ansify)

(eval-when (:load-toplevel :compile-toplevel :execute)

  (shadowing-export '(inspect describe describe-object
                      ))

  (export-from :ansify.ext
               '(#:inspect-object
                 ))

  ) ; end of `eval-when`


;;;; Utilities

(defun %prin* (x out)
  (if (si:*structurep x)
    (princ x out)
    (prin1 x out)))

(defconstant +undefined+ '#:undefined)

(defun maybe-undef (acc)
  (lambda (obj)
    (handler-case
        (funcall acc obj)
      (error (e)
        +undefined+))))



;;;; Inspect Protocol

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-inspectable (o s n)
    (declare (ignore n))
    (cond (*print-readably*
            (print-structure-readably o s))
          (t
            (print-unreadable-object (o s :type t :identity t)
              )))))

(defstruct (inspectable
            (:print-function print-inspectable))
  name
  label
  reader
  writer
  undefined-label
  )


(defun inspectable (name label &key reader writer accessor (undefined-label "<undefined>"))
  (when accessor
    (setf reader accessor
          writer accessor))
  (make-inspectable :name name :label label :reader reader :writer writer
                    :undefined-label undefined-label))



(defgeneric inspect-object (object)
  (:documentation
   "OBJECT の調べられるトコを inspectable オブジェクトのリストで返す。"))

(defmethod inspect-object ((cons cons))
  (list (inspectable 'car "car の値" :accessor 'car)
        (inspectable 'cdr "cdr の値" :accessor 'cdr)))



(defmethod inspect-object ((sym symbol))
  (list (inspectable 'name "名前" :reader 'symbol-name)
        (inspectable 'home-package "所属パッケージ" :reader 'symbol-package)
        (inspectable 'fdefinition "関数" :accessor (maybe-undef 'fdefinition))
        (inspectable 'symbol-value "変数としての値" :accessor (maybe-undef 'symbol-value))
        (inspectable 'symbol-plist "プロパティ" :reader 'symbol-plist)))

(defun inspect-test (object)
  (inspect-object object))



;;;; Describe

(defgeneric describe-object (object stream)
  (:documentation
   "OBJECT の説明を STREAM に出力する。"))

(defun %describe-inspectable-bit (object inspectable out)
  (let ((label (inspectable-label inspectable))
        (value (funcall (inspectable-reader inspectable) object)))
    (format out "~&~A:~16,4T" label)
    (if (eq value +undefined+)
      (princ (inspectable-undefined-label inspectable) out)
      (%prin* value out))
    (terpri out)))

(defmethod describe-object ((object t) stream)
  (let ((out (canonicalize-output-stream stream)))
    (format out "[~S] " (type-of object))
    (%prin* object out)
    (format out "~%------------------------------------------------------------------------~%")
    (dolist (x (inspect-object object))
      (%describe-inspectable-bit object x out))
    (format out "------------------------------------------------------------------------~%")
    ))



(defparameter *describe-depth* 0)

(defparameter *describe-depth-limit* 3)

(defun describe (object &optional (stream *standard-output*))
  (let ((out (canonicalize-output-stream stream)))
    (when (>= *describe-depth* *describe-depth-limit*)
      (%prin* object out))
    (let ((*describe-depth* (1+ *describe-depth*)))
      (describe-object object out)
      (values))))


(provide "ansify/inspect")
;;;; ansify/inspect.l ends here.
