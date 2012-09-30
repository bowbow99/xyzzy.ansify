;;; -*- mode: lisp; package: ansify.gfm -*-
;;;
;;; ansify/gfm.l --- Generic Function Modoki

;;; Code:


;;;; Package

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)

  (shadowing-export
   '(;; class APIs and names
     find-class class-of class-name subclassp
     generic-function builtin-function structure-object structure-class
     ;; GF/method
     defgeneric generic-function-p ensure-generic-function
     defmethod add-method next-method-p call-next-method
     ))

  (export-from :ansify.ext
               '(
                 ))
  )

(defpackage :ansify.gfm
  (:use :lisp :ansify.util)
  (:shadowing-import-from :ansify
    #:find-class #:class-of #:class-name #:subclassp
    #:generic-function #:builtin-function #:structure-object #:structure-class
    #:defgeneric #:generic-function-p #:ensure-generic-function
    #:defmethod #:add-method #:next-method-p #:call-next-method
    #:destructuring-bind #:fboundp #:fdefinition)
  (:import-from :ansify.ext
    #:optimize-type-check
    ))

(in-package :ansify.gfm)

(when (find-package :lisp-unit)
  (use-package :lisp-unit))



;;;; utilities

(defun typepred (typespec)
  (let ((form (optimize-type-check '#1=#:object typespec)))
    (if (and (= (length form) 2)
             (symbolp (first form))
             (eq (second form) '#1#))
      (first form)
      (coerce `(lambda (#1#) ,form) 'function))))

(defun !!type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun operator-form-p (thing operator)
  (and (consp thing) (eq (car thing) operator)))

(defun parse-body (body)
  "=> BODY, DECLS, DOC, INTERACTIVE"
  (let ((doc   nil)
        (decls '())
        (intr  nil))
    (labels ((%return (body)
               (return-from parse-body
                 (values body (nreverse decls) doc intr))))
      (do* ((rest body (cdr rest))
            (x #1=(first rest) #1#))
          ((null rest))
        (cond ((and (stringp x) (cdr rest) (not doc))
                (setf doc x))
              ((operator-form-p x 'declare)
                (push x decls))
              ((and (operator-form-p x 'ed:interactive) (not intr))
                (setf intr x))
              (t
                (%return rest))))
      (%return nil))))


;;; borrowed from ansify/destructuring-bind.l
(defmacro %assert-list-length (list min max)
  `(let ((#1=#:list ,list)
         (#2=#:min  ,min)
         (#3=#:max  ,max))
     (unless (listp #1#)
       (error 'type-error :datum #1# :expected-type 'list))
     (unless (or (null #2#) (zerop #2#) (nthcdr (1- #2#) #1#))
       (error 'too-few-arguments :datum #1#))
     (unless (or (null #3#) (<= (list-length #1#) #3#))
       (error 'too-many-arguments :datum #1#))
     #1#))

(defun lambda-list-spec (lambda-list)
  (let ((n-req 0)
        (n-opt 0)
        (rest? nil)
        (key?  nil)
        (state '&required))
    (dolist (x lambda-list)
      (if (member x lambda-list-keywords)
        (setf state x)
        (case state
          (&required (incf n-req))
          (&optional (incf n-opt))
          (&rest     (setf rest? t))
          (&key      (setf rest? t key? t))
          (&aux)
          (otherwise (error 'invalid-lambda-list
                            :datum lambda-list)))))
    (values n-req n-opt rest? key?)))

(defun lambda-list-nargs (lambda-list)
  (multiple-value-bind (n-req n-opt rest?)
      (lambda-list-spec lambda-list)
    (values n-req (if rest? nil (+ n-req n-opt)))))

(defun lambda-list-congruent-p (list1 list2)
  (equal (multiple-value-list (lambda-list-spec list1))
         (multiple-value-list (lambda-list-spec list2))))



;;;; Class system modoki
;;
;; - find-class
;; - class-of
;; - class-name
;; - subclassp

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-%BIC (BIC out depth)
    (declare (ignore depth))
    (format out "#<built-in-class: ~A>"
            (%BIC-name BIC))))

(defstruct (%BIC
            (:copier nil)
            (:print-function print-%BIC))
  "Built-in-class object modoki."
  name
  predicate
  superclasses
  subclasses)

(defun new-BIC (name predicate &key superclasses subclasses)
  (make-%BIC :name name :predicate predicate
             :superclasses superclasses
             :subclasses subclasses))

;;;;; BIC namespace

(defparameter *BIC-namespace* nil
  "An alist holding all the bulit-in-classes.")

(defparameter *the-t-class* nil
  "The `t` class, root of everything.")

(defparameter *the-structure-object-class* nil
  "The `structure-object` class (being a built-in-class).")

(defun %find-BIC (name &optional (errorp t))
  (or (cdr (assoc name *BIC-namespace*))
      (when errorp
        (error "Unknown built-in-class: ~S" name))))

(defmacro %define-BIC (name (&rest superclasses) &key predicate type)
  `(progn
     (let* ((superclasses
              (mapcar #'%find-BIC ',(or superclasses '(t))))
            (class
              (new-BIC ',name ,(or predicate `(typepred ,(or type `',name)))
                       :superclasses superclasses)))
       (setf *BIC-namespace* (acons ',name class *BIC-namespace*))
       (dolist (super superclasses)
         (push class (%BIC-subclasses super)))
       class)))

(defun %initialize-BICs ()

  ;; initialize BIC namespace
  (setf *the-t-class*   (new-BIC t (lambda (x) t))
        *BIC-namespace* (list (cons t *the-t-class*)))

  ;; each BICs
  (%define-BIC symbol ())
  (%define-BIC keyword (symbol))

  (%define-BIC sequence ())

  (%define-BIC list (sequence))
  (%define-BIC cons (list))
  (%define-BIC null (symbol list))

  (%define-BIC array ())
  (%define-BIC vector (sequence array))
  (%define-BIC string (vector))

  (%define-BIC number ())
  ;; FIXME: more number classes

  (%define-BIC function ())
  (%define-BIC generic-function (function) :predicate 'generic-function-p)
  (%define-BIC builtin-function (function) :predicate 'si:*builtin-function-p)
  ;; FIXME: macro-function being just a list...
  ;(%define-BIC macro-function (function))

  (%define-BIC character ())
  (%define-BIC stream ())
  (%define-BIC hash-table ())

  (%define-BIC structure-object () :predicate 'si:*structurep)
  (%define-BIC structure-class ()  :predicate 'si:*structure-definition-p)

  ;; FIXME: more BICs - specified by ANSI
  ;; FIXME: more BICs - xyzzy specific (buffer, regexp, marker,..)
  ;; FIXME: more BICs - extra (I want `buffer-stream` class)

  ;; finalize
  (setf *the-structure-object-class* (find-class 'structure-object))
  t)

;;;;; class APIs

(defun class-name (class)
  (cond ((si:*structure-definition-p class)
          (si:*structure-definition-name class))
        ((%BIC-p class)
          (%BIC-name class))
        ((!!type-error class 'class))))

(defun find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (or (get name 'si:structure-definition)
      (%find-BIC name nil)
      (when errorp
        (error "Class not found: ~S" name))))

(defun class-of (object)
  (if (si:*structurep object)
    (si:*structure-definition object)
    (labels ((search (BIC)
               (dolist (subclass (%BIC-subclasses BIC) BIC)
                 (when (funcall (%BIC-predicate subclass) object)
                   (return (search subclass))))))
      (let ((direct-class (search *the-t-class*)))
        (when (eq direct-class *the-t-class*)
          ;; NOET: No object should be a direct instance of `t`.
          ;; If so, most likely appropriate BIC is missing.
          (warn "BIC missing? ~S" object))
        direct-class))))

(defun %BIC-precedence-list (BIC)
  ;; NOTE: Doing topological-sort in place, hopefully
  (let ((sorted  '())
        (visited '()))
    (labels ((visit (BIC)
               (unless (member BIC visited :test #'eq)
                 (push BIC visited)
                 (dolist (super (%BIC-superclasses BIC))
                   (visit super))
                 (push BIC sorted))))
      (visit BIC)
      sorted)))

(defun subclassp (class1 class2)
  (when (symbolp class1)
    (setf class1 (find-class class1)))
  (when (symbolp class2)
    (setf class2 (find-class class2)))
  (cond ((eq class1 class2) t)
        ((si:*structure-definition-p class1)
          (or (eq class2 *the-structure-object-class*)
              (and (si:*structure-definition-p class2)
                   (si:*structure-subtypep class1 class2))))
        ((%BIC-p class1)
          (and (%BIC-p class2)
               (member class2 (%BIC-precedence-list class1) :test #'eq)))))

(%initialize-BICs)


;;;; Generic function modoki
;;
;; Generic function is a function which holding bunch of methods and dispatch
;; to it on invocation depending on class of given arguments.
;;
;; The list of methods (and some other stuff) are stored in lexical environment
;; of the closure.

;; FIXME: invoke GF interactively

(defstruct gf-info
  name
  lambda-list
  methods
  documentation)

(defmacro gf-info (gf)
  `(cdr (assoc 'GF-INFO (si:closure-variable ,gf))))

(defun generic-function-p (object)
  (and (si:*closurep object)
       (gf-info-p (gf-info object))))

;;;;; construction

(defun %make-generic-function (gf-name lambda-list documentation)
  (let ((GF-INFO
          ;; NOET: don't change this name (see macro `gf-info`)
          (make-gf-info :name gf-name
                        :lambda-list lambda-list
                        :documentation documentation)))
    (multiple-value-bind (nargs-min nargs-max)
        (lambda-list-nargs lambda-list)
      (si:*set-function-name
       (lambda (&rest #1=#:args)
         (%assert-list-length #1# nargs-min nargs-max)
         (%apply-method
          (or (appricable-method-list (gf-info-methods GF-INFO)
                                      #2=(subseq #1# 0 nargs-min)) ;just required portion
              (error "No appricable method: ~S: ~S" gf-name #2#))
          #1#))
       gf-name))))

;; FIXME: missing args for `ensure-generic-function`
;; - argument-precedence-order
;; - declare
;; - generic-function-class
;; - method-class
;; - method-combination
(defun ensure-generic-function
        (function-name &key documentation lambda-list environment)
  (cond
   ;; A. undefined function
   ((not (fboundp function-name))
     (setf (fdefinition function-name) (%make-generic-function
                                        function-name lambda-list documentation)
           ;; FIXME: Store docstring for generic-function
           ;; How can I do that on `(setf READER)` name?
           ;(documentation function-name) documentation
           ))
   ;; B. defined as ordinary function, macro, or special operator
   ((not (generic-function-p (fdefinition function-name)))
     ;; NOTE: `fmakunbound` via restart might be useful
     (error "Already fbound to something other than GF: ~S" function-name))
   ;; C. already a GF
   ((generic-function-p (fdefinition function-name))
     ;; FIXME: modify existing GF if necessary
     ;; See [CLHS: Function ENSURE-GENERIC-FUNCTION]
     )))
(setf (get 'ensure-generic-function 'ed:lisp-indent-hook) 1)

;;;;; macro `defgeneric`

(defun %parse-defgeneric-options (options name)
  "Classify OPTIONS given to `defgeneric`.
=> documentation  -- documentation string
=> method-descs   -- list of (METHOD-LAMBDA-LIST . BODY)"
  (let ((doc nil)
        (method-descs '()))
    (dolist (opt options)
      (case (car opt)
        (:documentation
          (when doc (error "More than one documentations: (defgeneric ~S ..)" name))
          (setf doc (second opt)))
        (:method
          ;; (:method METHOD-LAMBDA-LIST . BODY)
          (push (cdr opt) method-descs))
        (otherwise
          (error "Unknown (or unsupported) option: ~S: (defgeneric ~S ..)"
                 (car opt) name))))
    (values doc (nreverse method-descs))))

(defmacro defgeneric (name gf-lambda-list &rest options)
  (multiple-value-bind (doc method-descs)
      (%parse-defgeneric-options options name)
    `(progn
       (ensure-generic-function ',name
         :lambda-list ',gf-lambda-list
         :documentation ,doc)
       ;; FIXME: Add support for :method options
       )))
(set-indent 'defgeneric 'defun)


;;;; Method modoki
;;
;; Method is an object holding piece of code to executed, but method itself
;; is not a function. It lives in a GF's method list and invoked when GF got
;; applied to a set of arguments the method is specialized to.

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-*method (m out depth)
    (declare (ignore depth))
    (format out "#<method: ~S ~S #{~8S}>"
            (*method-name m)
            (*method-lambda-list m)
            (si:address-of m))))

(defstruct (*method
            (:print-function print-*method))
  name
  lambda-list
  specializers
  thunk
  documentation)

;;;;; construction

(defun %gen-specializer-form (spec)
  ;; (eql FORM) --> (cons 'eql FORM)          --> (eql . OBJECT)
  ;; CLASS-NAME --> (cons 'class 'CLASS-NAME) --> (class . CLASS-NAME)
  ;; #<CLASS>   --> (cons 'class 'CLASS-NAME) --> (class . CLASS-NAME)
  (cond
   ((and (listp spec) (eq (car spec) 'eql))
     `(cons 'eql ,(second spec)))
   ((symbolp spec)
     `(cons 'class ',spec))
   ((or (%BIC-p spec) (si:*structure-definition-p spec))
     `(cons 'class ',(class-name spec)))
   (t
     (error "Invalid specializer: ~S" spec))))

(defun %parse-method-lambda-list (method-lambda-list)
  "Parse METHOD-LAMBDA-LIST.
=> LAMBDA-LIST (ordinary one)
=> SPECIALIZERS"
  (let ((required-vars '())
        (specializers  '()))
    (labels ((%return (rest)
               (return-from %parse-method-lambda-list
                 (values (append (nreverse required-vars) rest)
                         (nreverse specializers)))))
      ;; loop over just required portion and collect parameter variables
      ;; and specializers separately
      (do ((rest method-lambda-list (cdr rest)))
          ((null rest))
        (let ((x (first rest)))
          (cond ((member x lambda-list-keywords)
                  (%return rest))
                ((consp x)
                  (push (first x)  required-vars)
                  (push (second x) specializers))
                ((symbolp x)
                  (push x required-vars)
                  (push t specializers))
                (t
                  (error 'invalid-lambda-list :datum method-lambda-list)))))
      (%return nil))))

(defparameter *appricable-method-list* nil)

(defun gen-make-method-form (name method-lambda-list body)
  (multiple-value-bind (lambda-list specializers)
      (%parse-method-lambda-list method-lambda-list)
    (multiple-value-bind (body decls doc intr)
        (parse-body body)
      `(make-*method
        :name ',name
        :specializers (list ,@(mapcar #'%gen-specializer-form specializers))
        :documentation ,doc
        :thunk (si:*set-function-name
                (lambda (&rest #0=#:method-args)
                  ,@(when intr `(,intr)) ;NOTE: interactive invocation not supported on GF side
                  ,@decls ;FIXME: is order of INTR and DECLS right?
                  (block ,name
                    (destructuring-bind ,lambda-list #0#
                      (labels ((next-method-p ()
                                 (and *appricable-method-list* t))
                               (call-next-method (&rest #1=#:next-args)
                                 (unless *appricable-method-list*
                                   (error "No next method: ~S" ',name))
                                 (%apply-method *appricable-method-list*
                                                (or #0# #1#))))
                        ,@body))))
                ',name)))))

;;;;; add method to GF

(defun %methods-agree-p (method1 method2)
  ;; NOTE: ANSI inconfirmance - no support for qualifier
  (equal (*method-specializers method1)
         (*method-specializers method2)))

(defun add-method (generic-function new-method)
  (let* ((gf-info (gf-info generic-function)))
    (setf (gf-info-methods gf-info)
          (cons new-method
                (delete-if (lambda (existing)
                             (%methods-agree-p existing new-method))
                           (gf-info-methods gf-info))))
    generic-function))

;;;;; macro `defmethod`

(defmacro defmethod (&environment env
                     name method-lambda-list &body body)
  (multiple-value-bind (*body decls doc intr)
      (parse-body body)
    `(progn
       (unless (and (fboundp ',name)
                    (generic-function-p (fdefinition ',name)))
         (ensure-generic-function ',name
           ;; FIXME: METHOD-LAMBDA-LIST is different form GF-LAMBDA-LIST
           ;; For now, the difference doesn't do any harm.
           :lambda-list ',method-lambda-list
           :documentation ,doc
           :environment ',env))
       (let ((gf-info (gf-info (fdefinition ',name))))
         (unless (lambda-list-congruent-p ',method-lambda-list
                                          (gf-info-lambda-list gf-info))
           (error "Method lambda list incongruent: ~S: ~S"
                  ',name ',method-lambda-list))
         (let ((method ,(gen-make-method-form name method-lambda-list body)))
           (add-method (fdefinition ',name) method)
           method)))))
(setf (get 'defmethod 'ed:lisp-indent-hook) 'defun)


;;;; Applying GF/method

(defun %method-appricable-p (method args)
  (every (lambda (spec arg)
           (let ((spec-type  (car spec))
                 (spec-datum (cdr spec))
                 (arg-class  (class-of arg)))
             (case spec-type
               (eql   (eql arg spec-datum))
               (class (subclassp arg-class (find-class spec-datum)))
               (t     (error "Invalid specializer: ~S" spec)))))
         (*method-specializers method)
         args))

(defun %specializer-more-specific-p (spec1 spec2)
  ;; NOTE: Assuming SPEC1 and SPEC2 are different
  (cond ((eq (car spec1) 'eql) t)
        ((eq (car spec2) 'eql) nil)
        ((subclassp (cdr spec1) (cdr spec2)) t)))

(defun %method-more-specific-p (method1 method2)
  (do ((specs1 (*method-specializers method1) (cdr specs1))
       (specs2 (*method-specializers method2) (cdr specs2)))
      ((null specs1) nil)
    (let ((spec1 (car specs1))
          (spec2 (car specs2)))
      (cond ((%specializer-more-specific-p spec1 spec2) (return t))
            ((%specializer-more-specific-p spec2 spec1) (return nil))))))

;; FIXME: cache list of appricable-methods
(defun appricable-method-list (all-methods args)
  (sort (remove-if-not (lambda (method)
                         (%method-appricable-p method args))
                       all-methods)
        #'%method-more-specific-p))

(defun %apply-method (method-list args)
  (let ((*appricable-method-list* (cdr method-list)))
    (apply (*method-thunk (car method-list)) args)))

;;;; next-method dummies

(defun next-method-p ()
  (error "Not in a method: ~S" 'next-method-p))

(defun call-next-method (&rest args)
  (error "Not in a method: ~S" 'call-next-method))


(provide "ansify/gfm")
;;; ansify/gfm.l ends here.
