;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; typespec+.l
;;;
;;; - typep accepts deftype'd typespec

(require "common-lisp/package")

(in-package :common-lisp)

(shadowing-export '(typep))

(defun typep (object typespec)
  (let (f)
    (multiple-value-bind (type r)
        (if (atom typespec)
          (values typespec nil)
          (values (car typespec) (cdr typespec)))
      (when (setq f (get type 'si::type-predicate))
        (return-from typep (funcall f object)))
      (when (setq f (get type 'si::type-specifier-list))
        (return-from typep (funcall f object r)))
      (when (setq f (get type 'si::structure-definition))
        (return-from typep
          (and (si:*structurep object)
               (si:*structure-subtypep (si:*structure-definition object) f)))))
    (let ((typespec~ (si:canonicalize-type typespec)))
      (unless (equal typespec~ typespec)
        (return-from typep (typep object typespec~))))
    nil))


;;;;
;;;; * Typespec Optimizer

(defparameter *typespec-optimizer-table* (make-hash-table))

(defmacro define-typespec-optimizer (spec (&rest args) &body body)
  `(setf (gethash ',spec *typespec-optimizer-table*)
         (lambda ,args ,@body)))

(define-typespec-optimizer t (object) t)
(define-typespec-optimizer nil (object) nil)

(define-typespec-optimizer satisfies (object pred)
  ;; TODO: reject lambda-expression
  ;; CLHS saying that PRED must be a symbol, and lambda expression is not allowed.
  ;; - http://www.lispworks.com/documentation/lw50/CLHS/Body/t_satisf.htm
  `(,pred ,object))

(define-typespec-optimizer member (object &rest items)
  (if (null items)
    nil
    ;; NOTE: Assuming whole typespec was quoted like: '(member foo bar baz)
    `(member ,object '(,@items))))

(define-typespec-optimizer eql (object item)
  `(eql ,object ,(if (constantp item) item `(quote ,item))))

(define-typespec-optimizer not (object type)
  (let ((form-1 (optimize-typep-call `(typep ,object ',type))))
    (case form-1
      ((t) nil)
      ((nil) t)
      (t `(not ,form-1)))))

(define-typespec-optimizer and (object &rest specs)
  (case (list-length specs)
    (0 t)
    (1 (optimize-typep-call `(typep ,object ',(car specs))))
    (t (let (($obj (gensym)))
         `(let ((,$obj ,object))
            (and ,@(mapcar (lambda (spec)
                             (optimize-typep-call `(typep ,$obj ',spec)))
                     specs)))))))

(define-typespec-optimizer or (object &rest types)
  (if (null types)
    nil
    (let (($obj (gensym)))
      `(let ((,$obj ,object))
         (or ,@(mapcar (lambda (type)
                         (optimize-typep-call `(typep ,$obj ',type)))
                 types))))))

(defun parse-typespec-form (form)
  (case (car form)
    (quote
     (let ((typespec (cadr form)))
       (cond ((consp typespec) (values (car typespec) (cdr typespec) t))
             ((symbolp typespec) (values typespec nil t))
             (t (error "Should not see me")))))
    ))

(defun optimize-typep-call (form)
  "Return a optimized version of typep form."
  (unless (and (listp form)
               (= (list-length form) 3)
               (member (first form) '(lisp:typep cl::typep)))
    (error "Malformed call for typep: ~S" form))
  (let ((object (second form)))
    (multiple-value-bind (typespec args success)
        (parse-typespec-form (third form))
      (or (when success
            (cond
             ((and (get typespec 'si::type-predicate)
                   (null args))
              `(,(get typespec 'si::type-predicate) ,object))
             ((gethash typespec *typespec-optimizer-table*)
              (return-from optimize-typep-call
                (apply (gethash typespec *typespec-optimizer-table*)
                       object args)))))
          form))))



(provide "typespec+")

;;; typespec+.l ends here.
