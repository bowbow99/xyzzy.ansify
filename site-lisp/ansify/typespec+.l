;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/typespec+.l --- type specifier （型指定子）を便利に
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/config"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)

  (shadowing-export '(#:typep
                      #:upgraded-complex-part-type
                      ))

  (export-from :ansify.ext
               '(#:canonicalize-typespec
                 #:optimize-type-check
                 #:typepred
                 ))

  ) ; end of `eval-when`


;;;; Fixes

;; NOTE: Typespec `eql` has broken definition on `si::deftype-definition`
;; (by `deftype`). For default `lisp:typep` which doesn't use definition
;; defined by `deftype` ignore this broken definition and doesn't cause
;; problem. When it is expanded by `si:canonicalize-type`, which expands
;; into a invalid typespec and result would be... wrong.
;;
;;     (si:canonicalize-type '(eql 3))
;;     => (member (3))
;;
;; I remove this broken definition. This change affects globaly, but
;; shouldn't cause any harm.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (remprop 'eql 'si::deftype-definition))

;; NOTE: Typespec `real` must accept LOWER-LIMIT and UPPER-LIMIT just like
;; other number types, but not implemented so in xyzzy.
;; ref: https://gist.github.com/1305573
;; ref: https://github.com/bowbow99/xyzzy.ansify/issues/19
(remprop 'real 'si::type-predicate)
(si::defpred-number real realp)

;; NOTE: Typespec `cons` must accept CAR-TYPE and CDR-TYPE, but not implemented
;; so in xyzzy.
(remprop 'cons 'si::type-predicate)
(si::defpred cons (object r)
  (and (consp object)
       (or (null r)
           (eql (first r) '*)
           (typep (car object) (first r)))
       (or (null (cdr r))
           (eql (second r) '*)
           (typep (cdr object) (second r)))))


;;;; Utilities

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


;;;; Misc

(defparameter *this-typespec* nil
  "Bound to whole typespec currently optimizing.")

(defun upgraded-complex-part-type (typespec &optional environment)
  (declare (ignore environment))
  (or (dolist (x '(bignum integer single-float double-float ratio
                   rational float real))
        (when (subtypep typespec x) (return x)))
      (error "Type `~S` is not a subtype of `real`." typespec)))

(defconstant +compound-only-typespecs+ '(and mod satisfies eql not values member or))

(defun !!invalid-type-specifier (typespec)
  (when *warn-on-invalid-typespec*
    (warn 'invalid-type-specifier :datum typespec)))

(defun !!undefined-type-specifier (typespec)
  (when *warn-on-undefined-typespec*
    (warn 'undefined-type-specifier :datum typespec)))

(defun split-typespec (typespec)
  "Return a symbol naming the type, and subsidiary info for compound typespec from TYPESPEC.
Signal an error of `lisp:bad-type-specifier` if TYPESPEC is invalid."
  (cond ((symbolp typespec)
         (when (member typespec +compound-only-typespecs+)
           (!!invalid-type-specifier typespec))
         (values typespec))
        ((consp typespec)
         (unless (symbolp (car typespec))
           (error 'bad-type-specifier :datum typespec))
         (values (car typespec) (cdr typespec)))
        (t
         (error 'bad-type-specifier :datum typespec))))


;;;; Function typep
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

ansify::type-specifier-list
---------------------------
Same as above, but overridden by ansify.

NOTE: Currently, only `complex` using this.


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

ansify::typespec-canonicalizer
------------------------------
<ansify specific>
Used to expand a derived typespec into completely a combination of primitive
types.
`si:canonicalize-type` is meant to do that, but it doesn't expands typespecs
given as arguments to another typespec, like `(and DERIVED-TYPESPEC ...)`, so
I made fixed version and function stored on `ansify::typespec-canonicalizer`
do the job for each types.
|#


;;;;; fixed canonicalize-type

(defmacro define-typespec-canonicalize (type lambda-list &body body)
  `(setf (get ',type 'typespec-canonicalizer)
         (lambda ,lambda-list ,@body)))

(define-typespec-canonicalize and (&rest typespecs)
  `(and ,@(mapcar #'canonicalize-typespec typespecs)))

(define-typespec-canonicalize or (&rest typespecs)
  `(or ,@(mapcar #'canonicalize-typespec typespecs)))

(define-typespec-canonicalize not (typespec)
  `(not ,(canonicalize-typespec typespec)))

(define-typespec-canonicalize satisfies (predicate)
  (unless (symbolp predicate)
    (!!invalid-type-specifier *this-typespec*))
  `(satisfies ,predicate))

(defun canonicalize-typespec (typespec &aux (*this-typespec* (or *this-typespec* typespec)))
  "Expand TYPESPEC into combination of primitive typespecs.
This is modified version of `si:canonicalize-type`.
The difference is that `canonicalize-typespec` does expand typespecs used
as argument for another typespec."
  (let (name args)
    (lisp::*loop
     (multiple-value-setq (name args) (split-typespec typespec))
     (acond ((get name 'si::deftype-definition)
             (setf typespec (apply it args)))
            ((get name 'typespec-canonicalizer)
             (return-from canonicalize-typespec
               (apply it args)))
            (t
             (return-from canonicalize-typespec
               (if (atom typespec) (list typespec) typespec)))))))


;;;;; overriding si::type-specifier-list

;; borrowed from lisp/typespec.l
(defmacro defpred (name (object r) &body body)
  `(setf (get ',name 'ansify::type-specifier-list)
         #'(lambda (,object ,r)
             (progn ,@body))))

(defpred complex (object r)
  (and (complexp object)
       (or (null r)
           (typep (realpart object) (upgraded-complex-part-type (car r))))))


;;;;; Funciton typep
(defun typep (object typespec)
  "Return true if OBJECT is type of TYPESPEC."
  (setf typespec (canonicalize-typespec typespec))
  (multiple-value-bind (type args) (split-typespec typespec)
    (acond
      ((get type 'si::type-predicate)
       (funcall it object))
      ((or (get type 'ansify::type-specifier-list)
           (get type 'si::type-specifier-list))
       (when (and (eql type 'satisfies)
                  (not (symbolp (first args))))
         (!!invalid-type-specifier typespec))
       (funcall it object args))
      ((get type 'si::structure-definition)
       (and (si:*structurep object)
            (si:*structure-subtypep (si:*structure-definition object) it)))
      (t
       (!!undefined-type-specifier typespec)
       nil))))


;;;; Optimize `typep` form

(defparameter *typep-optimizer-alist* nil)

(defmacro define-typep-optimizer (key (&rest args) &body body)
  `(setf *typep-optimizer-alist*
         (acons ',key (lambda (,@args) (block ,key ,@body))
                (delete ',key *typep-optimizer-alist* :key #'car))))

(defun find-typep-optimizer (key)
  (cdr (assoc key *typep-optimizer-alist*)))


;;; typespecs defined via `defpred` in lisp/typespec.l has their predicate
;;; function, but can be transformed into more straightforward form.

;; t, nil
(define-typep-optimizer t   (object) (*let `(#:object ,object) t))
(define-typep-optimizer nil (object) (*let `(#:object ,object) nil))

;; satisfies
(define-typep-optimizer satisfies (object pred)
  (unless (symbolp pred)
    (!!invalid-type-specifier *this-typespec*))
  `(,pred ,object))

;; member
(define-typep-optimizer member (object &rest items)
  `(member ,object '(,@items) :test #'eql))

;; eql
(define-typep-optimizer eql (actual expected)
  `(eql ,actual ',expected))

;; cons
(define-typep-optimizer cons (object &optional (car-type '*) (cdr-type '*))
  (*let `(#1=#:object ,object)
    (*and `((consp #1#)
            ,(if (eql car-type '*) t (optimize-type-check '(car #1#) car-type))
            ,(if (eql cdr-type '*) t (optimize-type-check '(cdr #1#) cdr-type))))))

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


;;;;; Numbers

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

(defopt-number real realp)
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


;;;;; Arrays

(defun expand-array-element-type-check (object element-type)
  (if (eql element-type '*) t
    `(eql (array-element-type ,object)
          ',(upgraded-array-element-type element-type))))


(defun expand-array-dimension-check (object dimension-spec)
  (cond ((eql dimension-spec '*) nil)
        ((atom dimension-spec) `((= (array-rank ,object) ,dimension-spec)))
        (t `((= (array-rank ,object) ,(length dimension-spec))
             ,@(let ((forms '()) (i 0))
                 (dolist (n dimension-spec)
                   (unless (eql n '*)
                     (push `(= (array-dimension ,object ,i) ,n) forms))
                   (incf i))
                 (nreverse forms))))))

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


;;;;; Function optimize-typep

(defun optimize-type-check (object-form typespec
                            &aux (*this-typespec* (or *this-typespec* typespec)))
  (multiple-value-bind (type args) (split-typespec typespec)
    (acond
      ((find-typep-optimizer type)
       (apply it object-form args))
      ((get type 'si::type-predicate)
       `(,it ,object-form))
      ((get type 'si::deftype-definition)
       (optimize-type-check object-form (si:canonicalize-type typespec)))
      ((get type 'si:structure-definition)
       (*let `(#1=#:object ,object-form)
         `(and (si:*structurep #1#)
               (si:*structure-subtypep (si:*structure-definition #1#)
                                       (get ',type 'si:structure-definition)))))
      (t
       (!!undefined-type-specifier typespec)
       `(typep ,object-form ',typespec)))))


;;;; Macro `typepred`

(defun typepred (typespec)
  "受け取ったオブジェクトが TYPESPEC の型かどうか判別する関数を返します。

引数:
  TYPESPEC: 型指定子

使用例:
  (typepred '(or string symbol))
  ;; == (lambda (#1=#:object) (or (stringp #1#) (symbolp #1#)))
  => #<lexical-closure: (anonymous)>

  (remove-if-not (typepred `(or function
                                (and symbol (satisfies fboundp))))
                 (list 'list 'symbol (lambda (x) x) (typepred 'fixnum)))
  => (list #<lexical-closure: <anonymous> #<function: *fixnump>)
"
  (let ((form (optimize-type-check '#1=#:object typespec)))
    (if (and (consp form)
             (= (length form) 2)
             (symbolp (first form))
             (eq (second form) '#1#))
      (first form)
      (coerce `(lambda (#1#) ,form) 'function))))


(provide "ansify/typespec+")
;;; ansify/typespec+.l ends here.
