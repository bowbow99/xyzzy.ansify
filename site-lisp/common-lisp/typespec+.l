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
      (when (setq f (get type 'si::structure-defintion))
        (return-from typep
          (si:*structure-subtypep (si:*structure-definition object) f))))
    (let ((typespec~ (si:canonicalize-type typespec)))
      (unless (equal typespec~ typespec)
        (return-from typep (typep object typespec~))))
    nil))


;;;;
;;;; * Typespec Optimizer

(defvar *typespec-optimizer-table* (make-hash-table))

(defmacro define-typespec-optimizer (spec (&rest args) &body body)
  `(setf (gethash ',spec *typespec-optimizer-table*)
         (lambda ,args ,@body)))

(define-typespec-optimizer and (object &rest specs)
  `(and ,@(mapcar (lambda (spec)
                    (optimize-typep-call `(typep ,object ',spec)))
            specs)))

(defun optimize-typep-call (form)
  "Return a form which produces same result with original FORM, but (hopefully) faster."
  (unless (and (listp form)
               (= (list-length form) 3)
               (member (car form) '(lisp:typep cl::typep)))
    (error "Malformed call for typep: ~S" form))
  (unless (eq (car (third form)) 'quote)
    (return-from optimize-typep-call form))
  (let ((typespec (cadr (third form)))
        (object (second form)))
    (or (cond
         ((member typespec si::typespec-alist :key #'car)
          `(,(cdr (assoc typespec si::typespec-alist)) ,object))
         ((and (consp typespec)
               (gethash (car typespec) *typespec-optimizer-table*))
          (let (($obj (gensym)))
            `(let ((,$obj ,object))
               ,(apply (gethash (car typespec) *typespec-optimizer-table*)
                       $obj (cdr typespec))))))
        form)))

;;;;
;;;; * Bugfixes

;;; by default, eql has both deftype-definition and type-specifier-list,
;;; and deftype-definition is unnecessary and broken.
;;;
;;;     (si:canonicalize-type '(eql 3))
;;;     => (member (3))
(remprop 'eql 'si::deftype-definition)


(provide "typespec+")

;;; typespec+.l ends here.
