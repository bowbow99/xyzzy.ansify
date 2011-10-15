;;; -*- mode: lisp; package: ansify.util -*-
;;;
;;; ansify/base/util.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base/package"))

(in-package :ansify.util)

(export '(;; list utilities
          single?
          proper-list-p
          ;; symbols
          mksym
          ;; binding macros
          with-ca/dr
          with-places
          ))


;;;; List Utilities

(defun single? (object)
  "Return true if OBJECT is a list with one element."
  (and (consp object) (null (cdr object))))

;; modified version of http://paste.lisp.org/display/24480
(defun proper-list-p (object)
  (if (null object) t
    (and (listp object)
         (do ((fast (cdr object) (cddr fast))
              (slow object (cdr slow)))
             ()
           (cond ((null fast) (return t))
                 ((atom fast) (return nil))
                 ((null (cdr fast)) (return t))
                 ((atom (cdr fast)) (return nil))
                 ((eql fast slow) (return nil)))))))


;;;; symbols

(defun mksym (fmt &rest args)
  (intern (apply #'format nil fmt args)))


;;;; Binding Macros

(defmacro with-ca/dr ((car-var cdr-var) cons &body body)
  `(let ((#1=#:cons ,cons))
     (let ((,car-var (car #1#))
           (,cdr-var (cdr #1#)))
       ,@body)))

(defmacro with-places ((&rest places) object &body body)
  `(let ((#1=#:object ,object))
     (let (,@(mapcar (lambda (place)
                       (if (consp place)
                         `(,(first place) (,(second place) #1#))
                         `(,place (,place #1#))))
               places))
       ,@body)))

(setf (get 'with-ca/dr 'ed:lisp-indent-hook) 2
      (get 'with-places 'ed:lisp-indent-hook) 2
      )


(provide "ansify/base/util")
;;; ansify/base/util.l ends here.
