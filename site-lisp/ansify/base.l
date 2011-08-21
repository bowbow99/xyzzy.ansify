;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/base.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base/package")
  (require "ansify/base/util")
  (require "ansify/base/anaph")
  )

(in-package :ansify)

(use-package :ansify.anaph)

(provide "ansify/base")
;;; ansify/base.l ends here.
