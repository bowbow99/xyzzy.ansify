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
                      ;#:otherwise
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
                    (message "元の値は ~S でした" ,key-var)
                    (list (read-value
                           (format nil "`~S` の値: " ',key-place))))
     (setf ,key-place #1#)
     (go ,tag)))

(defun make-case-test-form (keys var)
  (let ((op (if (consp keys) 'member 'eql)))
    `(,op ,var ',keys)))

(defun merge-keys (all-keys keys)
  (append all-keys (if (consp keys) keys (list keys))))

(defun expand-typecase-clause-1 (var clause)
  (with-ca/dr (type body) clause
    `(,(optimize-type-check var type)
      (progn ,@(cdr clause)))))

;;;; typecase family

;;; Macro `typecase`

(defun expand-typecase-clause (var clauses)
  (if (null clauses) nil
    (with-ca/dr (type body) (car clauses)
      (cond ((and (null (cdr clauses))
                  (member type '(t otherwise)))
             `((t ,@body)))
            (t
             `(,(expand-typecase-clause-1 var (car clauses))
               ,@(expand-typecase-clause var (cdr clauses))))))))

(defmacro typecase (keyform &rest clauses)
  `(let ((#1=#:key-object ,keyform))
     (cond ,@(expand-typecase-clause '#1# clauses))))

;;; Macro `etypecase`

(defun expand-etypecase (keyform clauses)
  (let ((types nil))
  `(let ((#1=#:key-object ,keyform))
     (cond ,@(mapcar (lambda (clause)
                       (push (car clause) types)
                       (expand-typecase-clause-1 '#1# clause))
                   clauses)
           (t ,(make-type-error-form '#1# `(or ,@(nreverse types))))))))

(defmacro etypecase (keyform &rest clauses)
  (expand-etypecase keyform clauses))

;;; Macro `ctypecase`

(defun expand-ctypecase (place clauses)
  (let ((types nil))
    `(prog (#1=#:key-object)
       #2=#:ctypecase-root
       (setf #1# ,place)
       (cond ,@(mapcar (lambda (clause)
                         (push (car clause) types)
                         (let ((x (expand-typecase-clause-1 '#1# clause)))
                           `(,(first x) (return ,(second x)))))
                     clauses)
             (t (restart-case
                    ,(make-type-error-form '#1# `(or ,@(nreverse types)))
                  ,(make-store-value-form '#1# place '#2#)))))))

(defmacro ctypecase (place &rest clauses)
  (expand-ctypecase place clauses))


;;;; case family

(defvar *case-warn-on-t/otherwise-in-middle* nil
  "*If non-nil, macro `case` signal a waring when `t` or `otherwise`
found at middle of clauses.
They should be `(t)` and `(otherwise)` respectively.")

(defun expand-case-clauses (var clauses)
  (labels ((otherwise-key-p (x)
             (member x '(otherwise t))))
    (with-ca/dr (keys body) (car clauses)
      (cond
       ;; last clauses (might be otherwise-clause)
       ((null (cdr clauses))
        (cond ((otherwise-key-p keys) `(progn ,@body))
              (t `(if ,(make-case-test-form keys var)
                    (progn ,@body)))))
       ;; non-last clause (cannot be a otherwise-clause)
       (t (when (and (otherwise-key-p keys)
                     *case-warn-on-t/otherwise-in-middle*)
            (warn "case - 最後じゃない clause が ~S になっとるよ" keys))
          `(if ,(make-case-test-form keys var)
             (progn ,@body)
             ,(expand-case-clauses var (cdr clauses))))))))

(defmacro case (key-form &rest clauses)
  `(let ((#1=#:key ,key-form))
     ,(expand-case-clauses '#1# clauses)))

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
