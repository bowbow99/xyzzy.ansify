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

(defvar *typespec-optimizer-table* (make-hash-table))

(defmacro define-typespec-optimizer (spec (&rest args) &body body)
  `(setf (gethash ',spec *typespec-optimizer-table*)
         (lambda ,args ,@body)))

(define-typespec-optimizer and (object &rest specs)
  `(and ,@(mapcar (lambda (spec)
                    (optimize-typep-call `(typep ,object ',spec)))
            specs)))

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
              (apply (gethash typespec *typespec-optimizer-table*)
                     object args))))
          form))))



(provide "typespec+")

;;; typespec+.l ends here.
