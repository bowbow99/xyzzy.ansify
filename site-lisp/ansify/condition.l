;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/condition.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)


;;;; local utilities

(defmacro define-simple-condition (parent)
  `(define-condition ,(mksym "simple-~A" parent) (,parent)
     (format-control format-arguments)
     (:report (lambda (c stream)
                (apply #'format stream
                       (si:*slot-value c :format-control)
                       (si:*slot-value c :format-arguments))))))


;;;; missing conditions

;;;;; style-warning

(define-condition style-warning (warning) ()
  (:report (lambda (sw stream)
             (format stream "(•AÅE)≤∏≈≤"))))

(define-simple-condition style-warning)


(provide "ansify/condition")
;;; ansify/condition.l ends here.
