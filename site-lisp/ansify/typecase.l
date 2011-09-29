;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/typecase.l
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; License:    MIT (see COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/typespec+"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:typecase
                      #:etypecase
                      #:ctypecase
                      #:otherwise
                      #:case
                      #:ecase
                      #:ccase
                      )))

;;; Indentation settings
#+xyzzy
(mapc (lambda (name)
        (setf (get name 'ed:lisp-indent-hook) 1))
      '(typecase
        etypecase
        ctypecase
        case
        ecase
        ccase))

;;; Macro `typecase`
(defmacro typecase (keyform &rest clauses)
  `(let ((#1=#:key-object ,keyform))
     (cond ,@(labels ((expand (clauses)
                        (cond ((null clauses) nil)
                              ((and (null (cdr clauses))
                                    (member (caar clauses) '(t otherwise)))
                               `((t ,@(cdar clauses))))
                              (t
                               `((,(optimize-type-check '#1# (caar clauses))
                                  (progn,@(cdar clauses)))
                                 ,@(expand (cdr clauses)))))))
               (expand clauses)))))

;;; Macro `etypecase`
(defmacro etypecase (keyform &rest clauses)
  `(let ((#1=#:key-object ,keyform))
     (cond ,@(mapcar (lambda (clause)
                       `(,(optimize-type-check '#1# (car clause))
                         (progn ,@(cdr clause))))
               clauses)
           (t (error 'type-error :datum #1#
                     :expected-type '(or ,@(mapcar #'car clauses)))))))

;;; Macro `ctypecase`
(defmacro ctypecase (place &rest clauses)
  `(prog (#1=#:key-object)
     #2=#:ctypecase-root
     (setf #1# ,place)
     (cond ,@(mapcar (lambda (clause)
                       `(,(optimize-type-check '#1# (car clause))
                         (return (progn ,@(cdr clause)))))
               clauses)
           (t (restart-case
                  (error 'type-error :datum #1#
                         :expected-type '(or ,@(mapcar #'car clauses)))
                (store-value (new)
                  :report ,(format nil "`~S' ÇÃílÇïœçXÇ∑ÇÈ" place)
                  :interactive
                  (lambda ()
                    (list (ed:read-value (format nil "`~S' ÇÃíl: " ',place))))
                  (setf ,place new)
                  (go #2#)))))))

(provide "ansify/typecase")
;;; ansify/typecase.l ends here.
