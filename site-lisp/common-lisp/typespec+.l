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
        (typep object typespec~)))
    nil))


(provide "typespec+")

;;; typespec+.l ends here.
