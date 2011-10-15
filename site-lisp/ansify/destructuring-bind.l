;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/destructuring-bind.l
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; License:    MIT (see COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/misc"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:destructuring-bind
                      #:&allow-other-keys
                      )))


;;;; utilities

(defun variablep (x)
  (and (symbolp x) (not (constantp x))))

(defmacro !invalid-lambda-list ()
  `(error 'invalid-lambda-list :datum *lambda-list*))


(defconstant &allow-other-keys '&allow-other-keys)
(defconstant &required '&required)

(defconstant +destructuring-lambda-list-keywords+
  '(&environment &whole &required &optional &body &rest &key &allow-other-keys &aux))

(defmacro %assert-list-length (list min max)
  `(let ((#1=#:list ,list)
         (#2=#:min ,min)
         (#3=#:max ,max))
     (unless (listp #1#)
       (error 'type-error :datum #1# :expected-type 'list))
     (unless (or (null #2#) (zerop #2#) (nthcdr (1- #2#) #1#))
       (error 'too-few-arguments :datum #1#))
     (unless (or (null #3#) (<= (list-length #1#) #3#))
       (error 'too-many-arguments :datum #1#))
     #1#))

(defmacro %ASSERT-LAMBDA-LIST (&rest tests)
  `(unless (and ,@tests)
     (error 'invalid-lambda-list :datum *lambda-list*)))


;;;; dynamic variables

(defparameter *lambda-list* nil)

(defparameter *whole-form-placeholder* nil)
(defparameter *whole-form* nil)
(defparameter *max-length* nil)
(defparameter *min-length* nil)

(defparameter *rest-form* nil)
(defparameter *rest-form-placeholder* nil)
(defparameter *rest-var* nil)

(defparameter *keyword-arguments-p* nil)
(defparameter *known-keywords* nil)
(defparameter *allow-other-keys-p* nil)


;;;; parsing lambda-list

(defparameter *state/parser-alist*
  '((&required . gen-&required-binding)
    (&optional . gen-&optional-binding)
    (&rest     . gen-&rest-binding)
    (&body     . gen-&rest-binding)
    (&key      . gen-&key-binding)
    (&aux      . gen-&aux-binding)
    ))

(defun parse-lambda-list (lambda-list form)
  (declare (type list lambda-list))
  (let* (;; for &whole
         (*whole-form-placeholder* nil)
         (*whole-form* nil)
         (*min-length* 0)
         (*max-length* 0)
         ;; for &rest/&key
         (*rest-var* nil)
         (*rest-form-placeholder* nil)
         (*rest-form* nil)
         (*keyword-arguments-p* nil)
         (*allow-other-keys-p* nil)
         (*known-keywords* nil)
         ;; misc
         (vars  nil)
         (forms nil)
         (tail  nil)
         (state '&required))
    (multiple-value-setq (vars forms tail lambda-list)
        (gen-&whole-binding lambda-list form))
    (progn
      (do ((lambda-list lambda-list (cdr lambda-list)))
          ((null lambda-list))
        (if (symbolp lambda-list)
          ; lambda-list was a dotted-list
          (setq lambda-list (list nil '&rest lambda-list))
          (with-ca/dr (x rest) lambda-list
            (if (member x +destructuring-lambda-list-keywords+)
              (progn
                (%ASSERT-LAMBDA-LIST (member x (cdr (member state +destructuring-lambda-list-keywords+))))
                (case x
                  ((&optional &key &aux)
                   (setq state x))
                  ((&rest &body)
                   (%ASSERT-LAMBDA-LIST (not (null rest))) ; no &rest parameter
                   (setq state x))
                  ((&allow-other-keys)
                   (%ASSERT-LAMBDA-LIST (eql state '&key)
                                        (or (null rest) (eql (car rest) '&aux)))
                   (setq *allow-other-keys-p* t))
                  (t
                   (error "Should not see me"))))
              (multiple-value-bind (vars% forms% tail%)
                  (funcall (or (cdr (assoc state *state/parser-alist*))
                               (error "No parser found for state `~S'."
                                      state))
                           x tail)
                (setq vars  (append vars vars%)
                      forms (append forms forms%)
                      tail  tail%))))))
      (setq forms (subst `(%assert-list-length
                           ,*whole-form* ,*min-length* ,*max-length*)
                         *whole-form-placeholder* forms)
            forms (subst (if (not *keyword-arguments-p*)
                           *rest-form*
                           `(%assert-keyword-list
                             ,*rest-form* ',*known-keywords* ,*allow-other-keys-p*))
                         *rest-form-placeholder* forms))
      (values vars forms))))

;;;;; &whole

;; NOTE: this one has bit different interface than other parse-*.
;; - 1st arg: takes entire LAMBDA-LIST rather than one PARAMETER in issue
;; - 4th value: returns (possibly modified) LAMBDA-LIST
(defun gen-&whole-binding (lambda-list form)
  (let ((placeholder (gensym "$WHOLE-FORM-")))
    (multiple-value-bind (param lambda-list)
        (if (eql (first lambda-list) '&whole)
          (values (second lambda-list) (cddr lambda-list))
          (values (gensym "whole-")    lambda-list))
      (multiple-value-bind (vars forms)
          (if (atom param)
            (progn
              (%ASSERT-LAMBDA-LIST (variablep param))
              (values (list param) (list placeholder)))
            (parse-lambda-list param placeholder))
        (setf *whole-form-placeholder* placeholder
              *whole-form* form)
        (values vars forms (first vars) lambda-list)))))

;;;;; required

(defun gen-&required-binding (param form)
  (multiple-value-bind (vars forms)
      (if (atom param)
        (progn
          (%ASSERT-LAMBDA-LIST (variablep param))
          (values (list param) (list `(car ,form))))
        (parse-lambda-list param `(car ,form)))
    (incf *min-length*)
    (incf *max-length*)
    (values vars forms `(cdr ,form))))

;;;;; &optional

(defun parse-&optional-parameter (param)
  (cond ((consp param)
         (let ((var     (first param))
               (default (second param))
               (sp-var  (if (cddr param)
                          (third param)
                          (gensym "supplied-p-"))))
           (%ASSERT-LAMBDA-LIST (null (cdddr param))
                                (variablep sp-var))
           (values var sp-var default)))
        ((variablep param)
         (values param (gensym "supplied-p-") nil))
        (t
         (!invalid-lambda-list))))

(defun gen-&optional-binding (param form)
  (multiple-value-bind (var sp-var default-form)
      (parse-&optional-parameter param)
    (let ((sp-form    `(not (null ,form)))
          (value-form `(if ,sp-var (car ,form) ,default-form)))
      (multiple-value-bind (vars forms)
          (cond ((consp var)     (parse-lambda-list var value-form))
                ((variablep var) (values (list var) (list value-form)))
                (t (!invalid-lambda-list)))
        (incf *max-length*)
        (values (list* sp-var  vars)
                (list* sp-form forms)
                `(cdr ,form))))))

;;;;; &rest

(defun %setup-rest-var/form (var form)
  (setf *max-length* nil
        *rest-var*   var
        *rest-form-placeholder* (gensym "$REST-FORM-")
        *rest-form* form))

(defun gen-&rest-binding (param form)
  (%ASSERT-LAMBDA-LIST (null *rest-var*))
  (multiple-value-bind (vars forms)
      (if (atom param)
        (progn
          (%ASSERT-LAMBDA-LIST (variablep param))
          (values (list param) (list form)))
        (parse-lambda-list param form))
    (%setup-rest-var/form (first vars) (first forms))
    (values (cons *rest-var* (cdr vars))
            (cons *rest-form-placeholder* (cdr forms))
            *rest-var*)))

;;;;; &key

(defun parse-&key-parameter (param)
  (if (atom param)
    ;; atomic form: VAR
    (progn
      (%ASSERT-LAMBDA-LIST (variablep param))
      (values (mkkwd param) param nil (gensym "keyarg-supplied-p-")))
    ;; compound form
    (let ((key/var (first param))
          (default (second param))
          (sp-var  (if (cddr param)
                     (third param)
                     (gensym "keyarg-supplied-p-"))))
      (%ASSERT-LAMBDA-LIST (null (cdddr param))
                           (variablep sp-var))
      (if (atom key/var)
        ;; (VAR DEFAULT SP-VAR)
        (progn
          (%ASSERT-LAMBDA-LIST (variablep key/var))
          (values (%keyword key/var) key/var default sp-var))
        ;; ((KEYSYM VAR/PATTERN) DEFAULT SP-VAR)
        (let ((key (first key/var))
              (var (second key/var)))
          (%ASSERT-LAMBDA-LIST (null (cddr key/var))
                               (symbolp key)
                               (or (variablep var) (consp var)))
          (values key var default sp-var))))))

(defun gen-&key-binding (param form)
  (multiple-value-bind (*vars *forms)
      (when (null *rest-var*)
        ;; NOTE: When &key started without &rest, use GENSYM as rest-var.
        (%setup-rest-var/form (gensym "all-keyargs-") form)
        (values (list *rest-var*)
                (list *rest-form-placeholder*)))
    (multiple-value-bind (keyword var default sp-var)
        (parse-&key-parameter param)
      (let* ((sp-var     (or sp-var (gensym "keyarg-supplied-p-")))
             (sp-form    `(get-properties ,form '(,keyword)))
             (value-form `(if ,sp-var (getf ,form ',keyword) ,default)))
        (multiple-value-bind (vars forms)
            (if (consp var)
              (let ((sublist-var (gensym "keyarg-sublist-")))
                (multiple-value-bind (vars forms)
                    (parse-lambda-list var sublist-var)
                  (values (list* sp-var  sublist-var vars)
                          (list* sp-form value-form  forms))))
              (values (list sp-var  var)
                      (list sp-form value-form)))
          (setf *keyword-arguments-p* t
                *known-keywords* (cons keyword *known-keywords*))
          (values (append *vars  vars)
                  (append *forms forms)
                  *rest-var*))))))

;;;;; &aux

(defun gen-&aux-binding (param form)
  (if (atom param)
    (progn
      (%ASSERT-LAMBDA-LIST (variablep param))
      (values (list param) (list nil)))
    (with-places (first second) param
      (%ASSERT-LAMBDA-LIST (<= (length param) 2)
                           (variablep first))
      (values (list first) (list second)))))


;;;; Macro destructuring-bind

(defmacro destructuring-bind (lambda-list expr &body body)
  (multiple-value-bind (vars values)
      (let ((*lambda-list* lambda-list))
        (parse-lambda-list lambda-list expr))
    `(let* (,@(mapcar #'list vars values))
       ,@body)))
(set-indent 'destructuring-bind 2)


(provide "ansify/destructuring-bind")
;;; ansify/destructuring-bind.l ends here.
