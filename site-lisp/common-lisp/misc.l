;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/misc.l
;;;


(require "common-lisp/package")

(export '(constantly
          ))

(defun constantly (value)
  (lambda (&rest #:ignore) value))


(provide "common-lisp/misc")

;;; common-lisp/misc.l ends here.
