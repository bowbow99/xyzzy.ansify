;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/base/util.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base/package"))

(in-package :ansify)

;;;;
;;;; * List Utilities

(defun single? (object)
  "Return true if OBJECT is a list with one element."
  (and (consp object) (null (cdr object))))

;;;;
;;;; * Binding Macros

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
