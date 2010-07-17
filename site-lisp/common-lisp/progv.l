;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/progv.l
;;;
;;; [CLHS: Special Operator PROGV]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Body/s_progv.htm
;;;

(require "common-lisp/package")

(in-package :common-lisp)

(export '(progv))

(defconstant +unbound-value+ (gensym))

(defun %safe-symbol-value (symbol)
  (if (boundp symbol) (symbol-value symbol) +unbound-value+))

(defmacro progv (symbols values &body body)
  (let (($syms (gensym))
        ($vals (gensym))
        ($origs (gensym)))
    `(let ((,$syms ,symbols)
           (,$vals ,values)
           (,$origs (make-hash-table)))
       (unwind-protect
           (progn
             (mapc (lambda (sym val)
                     (setf (gethash sym ,$origs) (%safe-symbol-value sym)
                           (symbol-value sym) val))
                   ,$syms ,$vals)
             ,@body)
         (maphash (lambda (sym val)
                    (if (eq val +unbound-value+)
                        (makunbound sym)
                      (setf (symbol-value sym) val)))
                  ,$origs)))))

(provide "common-lisp/progv")

;;; common-lisp/progv.l ends here.
