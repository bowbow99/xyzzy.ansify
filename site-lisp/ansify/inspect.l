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

(defun %prin*-to-string (x)
  (with-output-to-string (s) (%prin* x s)))

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
  type
  )

(defun %inspectable (name label type &key reader writer accessor (undefined-label "[undefined]"))
  (when accessor
    (setf reader accessor  writer accessor))
  (make-inspectable :name name  :label label  :reader reader  :writer writer
                    :undefined-label undefined-label  :type type))


(defgeneric inspect-object (object)
  (:documentation
   "OBJECT の調べられるトコを inspectable オブジェクトのリストで返す。"))

(defmethod inspect-object ((cons cons))
  (list (inspectable 'car "car の値" :accessor 'car)
        (inspectable 'cdr "cdr の値" :accessor 'cdr)))

(defmacro definspect (class &rest specs)
  `(defmethod inspect-object ((,class ,class))
     (list ,@(mapcar (lambda (spec)
                       (apply (lambda (name label type &rest args)
                                `(%inspectable ',name ,label ',type ,@args))
                              spec))
                     specs))))
(setf (get 'definspect 'ed:lisp-indent-hook) 'defun
      (get 'definspect 'ed::lisp-indent-clause) 2)

(definspect symbol
  (name         "シンボル名"      string    :reader 'symbol-name)
  (home-package "所属パッケージ"  package   :reader 'symbol-package)
  (fdefinition  "関数定義"        function  :accessor (maybe-undef 'fdefinition))
  (value        "変数としての値"  t         :accessor (maybe-undef 'symbol-value))
  (symbol-plist "プロパティ"      plist     :reader 'symbol-plist))

(definspect package
  (name         "パッケージ名" string
    :reader 'package-name)
  (nicknames    "ニックネーム" (list-of string)
    :accessor 'package-nicknames)
  (using        "use-package されているパッケージ" (list-of package)
    :reader 'package-use-list)
  (used-by      "use-package しているパッケージ" (list-of package)
    :reader 'package-used-by-list)
  (documentation "説明文" docstring
    :reader (lambda (x) (or (documentation package t) +undefined+))
    :undefined-label "[無し]")
  ;external-symbols
  ;internal-symbols
  )




;;;; Describe

(defgeneric describe-object (object stream)
  (:documentation
   "OBJECT の説明を STREAM に出力する。"))

(defun %describe-inspectable-bit (object inspectable out)
  (let ((label (inspectable-label inspectable))
        (value (funcall (inspectable-reader inspectable) object))
        (type  (inspectable-type inspectable)))
    (cond ((eq value +undefined+)
            (format out "~&~A:~16,4T~A~%" label (inspectable-undefined-label inspectable)))
          ((listp type)
            (lisp:case (car type)
              (list-of
                (format out "~&~A:~%~{    ~A~%~}"
                        label
                        (mapcar #'%prin*-to-string value)))))
          ((symbolp type)
            (lisp:case type
              (plist
                (format out "~&~A:~%" label)
                (do ((rest value (cddr rest)))
                    ((null rest))
                  (format out "    ~S~20,4T~A~%"
                          (first rest)
                          (%prin*-to-string (second rest)))))
              (docstring
                (format out "~&~A:~%~{    ~A~%~}" label
                        (ed:split-string value #\LFD)))
              (otherwise
                (format out "~&~A:~16,4T~A~%" label (%prin*-to-string value))))))))

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
