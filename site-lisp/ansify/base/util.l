;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/base/util.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base/package"))

(in-package :ansify)

;;;;
;;;; * List Utilities

(defun single? (object)
  "Return true if OBJECT is a list with one element."
  (and (consp object) (null (cdr object))))


(provide "ansify/base/util")
;;; ansify/base/util.l ends here.
