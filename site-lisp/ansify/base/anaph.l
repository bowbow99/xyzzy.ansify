;;; -*- mode: lisp; package: ansify.anaph -*-
;;;
;;; ansify/base/anaph.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(defpackage :ansify.anaph
  (:use :lisp))

(in-package :ansify.anaph)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(aif
            awhen
            awhile
            acond
            aand
            )))

(defun flatten (tree)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t
                    (rec (car x) (rec (cdr x) acc))))))
    (rec tree nil)))

(defun find-it-symbol (form)
  (or (find-if (lambda (x)
                 (and (symbolp x)
                      (string-equal "it" (symbol-name x))))
        (flatten form))
      'it))


(defmacro aif (test then-form &optional else-form)
  (let ((it (find-it-symbol then-form)))
    `(let ((,it ,test))
       (if ,it ,then-form ,else-form))))

(defmacro awhen (test &body body)
  (let ((it (find-it-symbol body)))
    `(let ((,it ,test))
       (if ,it (progn ,@body)))))

(defmacro awhile (test &body body)
  (let ((it (find-it-symbol body)))
    `(let ((,it nil))
       (while (setq ,it ,test)
         ,@body))))

(defmacro acond (&rest clauses)
  (let ((it (find-it-symbol (mapcar #'cdr clauses))))
    (labels ((expand-clauses (clauses)
               (cond ((null clauses) nil)
                     ((and (null (cdr clauses))
                           (eql (caar clauses) t))
                      (if (null (cdar clauses))
                        nil
                        `(progn ,@(cdar clauses))))
                     (t
                      `(if (setq ,it ,(caar clauses))
                         (progn ,@(cdar clauses))
                         ,(expand-clauses (cdr clauses)))))))
      `(let ((,it nil))
         ,(expand-clauses clauses)))))

(defmacro aand (&rest tests)
  (let ((it (find-it-symbol tests)))
    `(let (,it)
       (and ,@(mapcar (lambda (test)
                        `(setf ,it ,test))
                    tests)))))

#+xyzzy
(setf (get 'aif 'ed:lisp-indent-hook) 1
      (get 'awhen 'ed:lisp-indent-hook) 1
      (get 'awhile 'ed:lisp-indent-hook) 1
      )

(provide "ansify/base/anaph")
;;; ansify/base/anaph.l ends here.
