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

(export '(;; misc
          set-indent
          import-from
          export-from
          ;; list utilities
          single?
          proper-list-p
          flatten
          ;; symbols
          mksym
          mkkwd
          ;; binding macros
          with-ca/dr
          with-places
          ))


;;;; misc

(defun set-indent (operator indent)
  (setf (get operator 'ed::lisp-indent-hook) indent))

(defun import-from (package names)
  "Import symbols named NAMES from PACKAGE.
Symbols to be imported must be exported from PACKAGE."
  (import (mapcar (lambda (name)
                    (multiple-value-bind (symbol class)
                        (find-symbol (string name) package)
                      (case class
                        (:external symbol)
                        ((:internal :inherited)
                         (error "import-from - `~S` is not an external symbol in package ~S"
                                symbol package))
                        (t
                         (error "import-from - `~A` not found in package ~S"
                                name package)))))
                name)))

(defun export-from (package names)
  "Export symbols named NAMES from PACKAGE.
Symbols to be exported are interned into PACKAGE first."
  (export (mapcar (lambda (name)
                    (intern (string name) package))
                names)
          package))


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

(defun flatten (tree)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t
                    (rec (car x) (rec (cdr x) acc))))))
    (rec tree nil)))


;;;; symbols

(defun mksym (fmt &rest args)
  (intern (apply #'format nil fmt args)))

(defun mkkwd (name)
  (intern (string name) :keyword))


;;;; Binding Macros

(defmacro with-ca/dr ((car-var cdr-var) cons &body body)
  `(let ((#1=#:cons ,cons))
     (let ((,car-var (car #1#))
           (,cdr-var (cdr #1#)))
       ,@body)))
(set-indent 'with-ca/dr 2)

(defmacro with-places ((&rest places) object &body body)
  `(let ((#1=#:object ,object))
     (let (,@(mapcar (lambda (place)
                       (if (consp place)
                         `(,(first place) (,(second place) #1#))
                         `(,place (,place #1#))))
               places))
       ,@body)))
(set-indent 'with-places 2)


;;;; development/debugging

(export '(@debug
          ))

(defmacro @debug (datum &rest args)
  (cond ((stringp datum)
         `(format t ,datum ,@args))
        (t
         `(format t "~&~:{* ~S => ~S~%~}"
                (list ,@(mapcar (lambda (form)
                                  `(list ',form ,form))
                              (cons datum args)))))))

(provide "ansify/base/util")
;;; ansify/base/util.l ends here.
