;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/typespec+.l --- type specifier （型指定子）を便利に
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:compile-toplevel :execute)
  (require "cmu_loop"))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

;(defconstant +original-typep-function+ #'typep)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:typep
                      )))

(export '(*warn-on-undefined-typespec*
          ))

;;;;
;;;; * User Configuration

(defvar *warn-on-undefined-typespec* nil
  "If non-nil, warn when undefined typespec used on `typep` or `optimize-type-check.")

;;;;
;;;; * Utilities

(defun *let (binding &rest body)
  (let ((var (first binding))
        (object (second binding)))
    (if (or (symbolp object)
            (constantp object))
      (labels ((rec (form)
                 (cond ((eql form var) object)
                       ((atom form) form)
                       (t (cons (rec (car form))
                                (rec (cdr form)))))))
        (if (single? body) (rec (car body)) `(progn ,@(mapcar #'rec body))))
      `(let ((,var ,object)) ,@body))))
#+xyzzy
(setf (get '*let 'ed:lisp-indent-hook) 1)

(defun *and (tests)
  (let ((tests% (remove-if (lambda (x) (and (constantp x) x)) tests)))
    (case (length tests%)
      (0 t)
      (1 (car tests%))
      (t `(and ,@tests%)))))

(defun *or (&rest tests)
  (cond ((single? tests) (car tests))
        (t `(or ,@tests))))

(defun both (a b &optional (pred #'identity))
  (and (funcall pred a) (funcall pred b)))

;;;;
;;;; * Funciton typep
#|
Implementation Note
===================

    (typep OBJECT TYPE)
    (typep OBJECT (TYPE . ARGS))

`typep` decide how to check whether OBJECT is type of TYPE from propery of
symbol given as TYPE.


si::type-predicate
------------------
If TYPE has this property, it must be a function which takes one argument,
OBJECT, and return true if the OBJECT is type of TYPE.

    (typep OBJECT TYPE) == (PREDICATE OBJECT)


si::type-specifier-list
-----------------------
If TYPE has this property, it must be a function which takes two arguments,
OBJECT and ARGS, and return true if the OBJECT is type of TYPE and satisfies
requirement specified by ARGS.

    (typep OBJECT (TYPE . ARGS)) == (<function> OBJECT ARGS)

NOET: These are defined via `si::defpred` in lisp/typespec.l.

NOTE: Some compound type specifiers accepts `nil` as ARGS. In other words,
all arguments are optional. In most cases, it can be just a symbol (with-
out parenthesis) if there's no argument. But some type specifiers like `and`
doesn't allowed to be just a symbol.


si::deftype-definition
----------------------
A type specifier defined via `deftype` has this property. It is a function
which takes arguments specified in `deftype` form, and return expansion of
defined typespec. Returned expansion should be a valid typespec.

    (typep OBJECT (TYPE . ARGS)) == (typep OBJECT (apply <expander> ARGS))

NOTE: xyzzy's default `typep` doesn't support these typespecs, and you need
to use `si:canonicalize-type` to expand them.


si::structure-definition
------------------------
Structure name defined via `defstruct` has this property, and its value is
a object called "structure-definition". It can be compared with "structure-
object".

    (type OBJECT STRUCTURE)
    == (and (si:*structurep OBJECT)
            (si::*structure-subtypep (si:*structure-definition OBJECT)
                                     #<structure-definition: STRUCTURE>))


|#

(defun typep (object typespec)
  "Return true if OBJECT is type of TYPESPEC."
  (multiple-value-bind (type args)
      (if (consp typespec)
        (values (car typespec) (cdr typespec))
        (values typespec nil))
    (acond
      ((get type 'si::type-predicate)
       (funcall it object))
      ((get type 'si::type-specifier-list)
       ;; TODO: some typespecs (ie. `and`, `or`) are not allowed to be a symbol
       (funcall it object args))
      ((get type 'si::structure-definition)
       (and (si:*structurep object)
            (si:*structure-subtypep (si:*structure-definition object) it)))
      ((get type 'si::deftype-definition)
       (typep object (si:canonicalize-type typespec)))
      (t
       (when *warn-on-undefined-typespec*
         (warn "type specifier じゃないみたい: ~S" typespec))
       nil))))

;;;;
;;;; * Optimize `typep` form

(defparameter *typep-optimizer-table* (make-hash-table))

(defmacro define-typep-optimizer (name (&rest args) &body body)
  `(setf (gethash ',name *typep-optimizer-table*)
         (lambda (,@args) (block ,name ,@body))))


;;; typespecs defined via `defpred` in lisp/typespec.l has their predicate
;;; function, but can be transformed into more straightforward form.

;; t, nil
(define-typep-optimizer t   (object) (*let `(#:object ,object) t))
(define-typep-optimizer nil (object) (*let `(#:object ,object) nil))

;; satisfies
(define-typep-optimizer satisfies (object pred)
  ;; TODO: CLHS explicitly denying use of lambda-expression as pred
  `(,pred ,object))

;; member
(define-typep-optimizer member (object &rest items)
  `(member ,object '(,@items) :test #'eql))

;; eql
(define-typep-optimizer eql (actual expected)
  `(eql ,actual ',expected))

;; not
(define-typep-optimizer not (object typespec)
  (*let `(#1=#:object ,object)
    (let ((optimized (optimize-type-check '#1# typespec)))
      (cond ((eql optimized t) nil)
            ((eql optimized nil) t)
            (t `(not ,optimized))))))

;; and
(define-typep-optimizer and (object &rest typespecs)
  (if (null typespecs)
    ;; `*let` removes OBJECT form if its a constant or a variable (not necessary to evaluate)
    (*let `(#:object ,object) t)
    (*let `(#1=#:object ,object)
      (*and (mapcar (lambda (typespec)
                      (optimize-type-check '#1# typespec))
              typespecs)))))

;; or
(define-typep-optimizer or (object &rest typespecs)
  (if (null typespecs)
    (*let `(#:object ,object) nil)
    (*let `(#1=#:object ,object)
      (apply #'*or
             (mapcar (lambda (typespec)
                       (optimize-type-check '#1# typespec))
               typespecs)))))

;; standard-char
(define-typep-optimizer standard-char (object)
  (*let `(#1=#:object ,object)
    `(and (characterp #1#)
          (standard-char-p #1#))))

;; extended-character
(define-typep-optimizer extended-character (object)
  (*let `(#:object ,object) nil))


;;;
;;; Numbers

(defun expand-number-range-check (object low high)
  (cond ((both low high #'numberp)
         `((<= ,low ,object ,high)))
        ((both low high #'consp) ; should be a singleton list of number
         `((< ,(car low) ,object ,(car high))))
        (t
         `(,(cond ((eql low '*)   t)
                  ((numberp low)  `(<= ,low ,object))
                  ((consp low)    `(< ,(car low) ,object)))
           ,(cond ((eql high '*)   t)
                  ((numberp high) `(<= ,object ,high))
                  ((consp high)   `(< ,object ,(car high))))))))

(defmacro defopt-number (name predicate)
  `(define-typep-optimizer ,name (object &optional (low '*) (high '*))
     (if (both low high (lambda (x) (eql x '*)))
       `(,',predicate ,object)
       (*let `(#1=#:object ,object)
         (*and (list* `(,',predicate #1#)
                      (expand-number-range-check '#1# low high)))))))

(defopt-number integer integerp)
(defopt-number float floatp)
(defopt-number rational rationalp)
(defopt-number short-float short-float-p)
(defopt-number single-float single-float-p)
(defopt-number long-float long-float-p)
(defopt-number double-float double-float-p)

;; complex
(define-typep-optimizer complex (object &optional (typespec '*))
  (if (eql typespec '*)
    `(complexp ,object)
    (*let `(#1=#:object ,object)
      (*and (list '(complexp #1#)
                  (if (eql typespec '*) t
                    (optimize-type-check '(realpart #1#) typespec)))))))


;;;
;;; Arrays

(defun expand-array-element-type-check (object element-type)
  (if (eql element-type '*) t
    `(eql (array-element-type ,object)
          ',(upgraded-array-element-type element-type))))


(defun expand-array-dimension-check (object dimension-spec)
  (cond ((eql dimension-spec '*) nil)
        ((atom dimension-spec) `((= (array-rank ,object) ,dimension-spec)))
        (t `((= (array-rank ,object) ,(length dimension-spec))
             ,@(loop for n in dimension-spec
                     for i = 0 then (1+ i)
                 collect (if (eql n '*) t
                           `(= (array-dimension ,object ,i) ,n)))))))

(defun expand-vector-size-check (object size)
  (if (eql size '*) t
    `(= (array-dimension ,object 0) ,size)))

(defmacro defopt-array (name predicate &rest opts)
  (let ((args (mapcar (lambda (x) (intern (string x))) opts)))
    `(define-typep-optimizer ,name (object &optional ,@(mapcar (lambda (x) `(,x '*)) args))
       (if (every (lambda (x) (eql x '*)) (list ,@args))
         `(,',predicate ,object)
         (*let `(#1=#:object ,object)
           (*and (list* `(,',predicate #1#)
                        ,@(when (member :element-type opts)
                            (list '(expand-array-element-type-check '#1# element-type)))
                        ,@(when (member :size opts)
                            (list '(expand-vector-size-check '#1# size)))
                        ,(when (member :dimension-spec opts)
                           '(expand-array-dimension-check '#1# dimension-spec)))))))))

;; array  [element-type] [dimension-spec]
(defopt-array array arrayp :element-type :dimension-spec)

;; simple-array  [element-type] [dimension-spec]
(defopt-array simple-array si:*simple-array-p :element-type :dimension-spec)

;; vector  [element-type] [size]
(defopt-array vector vectorp :element-type :size)

;; simple-vector  [size]
(defopt-array simple-vector simple-vector-p :size)

;; string  [size]
(defopt-array string stringp :size)

;; simple-string  [size]
(defopt-array simple-string simple-string-p :size)


;;; * Function optimize-typep

(defun optimize-type-check (object-form typespec
                            &optional (no-warn (null *warn-on-undefined-typespec*)))
  (multiple-value-bind (type args)
      (if (consp typespec)
        (values (car typespec) (cdr typespec))
        (values typespec nil))
    (acond
      ((gethash type *typep-optimizer-table*)
       (apply it object-form args))
      ((get type 'si::type-predicate)
       `(,it ,object-form))
      ((get type 'si::type-specifier-list)
       `(,it ,object-form '(,@args)))
      ((get type 'si:structure-definition)
       `(let ((#1=#:object ,object-form))
          (and (si:*structurep #1#)
               (si:*structure-subtypep (si:*structure-definition #1#)
                                       (get ',type 'si:structure-definition)))))
      ((get type 'si::deftype-definition)
       (optimize-type-check object-form (si:canonicalize-type typespec)))
      (t
       (unless no-warn
         (warn "type specifier じゃないみたい: ~S" typespec))
       `(typep ,object-form ',typespec)))))

;;;;
;;;; * Macro `typepred`
(defmacro typepred (typespec)
  "Create a function which takes an object and return true if the object is type of TYPESPEC."
  (let ((form (optimize-type-check '#1=#:object typespec)))
    (if (and (= (length form) 2)
             (eql (second form) '#1#))
      `#',(car form)
      `(lambda (#1#) ,form))))

(provide "ansify/typespec+")
;;; ansify/typespec+.l ends here.
