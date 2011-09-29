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

;;;; helper functions


(defun make-type-error-form (datum expected)
  `(error 'type-error :datum ,datum
          :expected-type ',expected))

(defun make-store-value-form (key-var key-place tag)
  `(store-value (#1=#:new-value)
     :report ,(format nil "`~S` の値を変更する" key-place)
     :interactive (lambda ()
                    (message "元の値 => ~S" ,key-var)
                    (list (ed:read-value
                           (format nil "`~S` の値: " ',key-place))))
     (setf ,key-place #1#)
     (go ,tag)))

(defun make-case-test-form (keys var)
  (let ((op (if (consp keys) 'member 'eql)))
    `(,op ,var ',keys)))

(defun merge-keys (all-keys keys)
  (append all-keys (if (consp keys) keys (list keys))))

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
           (t ,(make-type-error-form '#1# `(or (mapcar #'car clausese)))))))

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
                  :report ,(format nil "`~S' の値を変更する" place)
                  :interactive
                  (lambda ()
                    (list (ed:read-value (format nil "`~S' の値: " ',place))))
                  (setf ,place new)
                  (go #2#)))))))

;;; Macro `case`
;;
;; - otherwise is supported

(defmacro case (key-form &rest clauses)
  `(let ((#1=#:key ,key-form))
     ,(labels ((expand-clauses (clauses)
                 (let ((keys (car (car clauses)))
                       (body (cdr (car clauses))))
                   (if (null (cdr clauses))
                     ;; last clause
                     (if (member keys '(otherwise t) :test #'eq)
                       `(progn ,@body)
                       `(if (,(if (consp keys) 'member 'eql) #1# ',keys)
                          (progn ,@body)))
                     ;; non-last clauses
                     (progn
                       (when (member keys '(otherwise t) :test #'eq)
                         (warn "case - 最後じゃない clause が ~S になってるよ" keys))
                       `(if (,(if (consp keys) 'member 'eql) #1# ',keys)
                          (progn ,@body)
                          ,(expand-clauses (cdr clauses))))))))
        (expand-clauses clauses))))



;;; Macro `ecase`

(defun expand-ecase-clauses (var clauses &optional all-keys)
  (cond ((null clauses)
         (make-type-error-form var `(member ,@all-keys)))
        (t
         (let ((keys (car (car clauses)))
               (body (cdr (car clauses))))
           `(if ,(make-case-test-form keys var)
              (progn ,@body)
              ,(expand-ecase-clauses var (cdr clauses)
                                     (merge-keys all-keys keys)))))))

(defmacro ecase (key-form &rest clauses)
  `(let ((#1=#:key ,key-form))
     ,(expand-ecase-clauses '#1# clauses)))

;;; Macro `ccase`

(defun expand-ccase (key-place clauses)
  `(prog (#1=#:key-value)
     #2=#:ccase-root
     (setf #1# ,key-place)
     ,(labels ((expand-clauses (clauses &optional all-keys)
                 (if (null clauses)
                   `(restart-case
                        ,(make-type-error-form '#1# `(member ,@all-keys))
                      ,(make-store-value-form '#1# key-place '#2#))
                   (let ((keys (car (car clauses)))
                         (body (cdr (car clauses))))
                     `(if ,(make-case-test-form keys '#1#)
                        (return (progn ,@body))
                        ,(expand-clauses (cdr clauses)
                                         (merge-keys all-keys keys)))))))
        (expand-clauses clauses))))

(defmacro ccase (key-place &rest clauses)
  (expand-ccase key-place clauses))

(provide "ansify/typecase")
;;; ansify/typecase.l ends here.
