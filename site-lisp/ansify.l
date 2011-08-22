;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify.l --- ANSI Common Lisp Ç…Ç†Ç¡Çƒ xyzzy Ç…Ç»Ç¢Ç‡ÇÃãlÇﬂçáÇÌÇπ
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.00.00
;;; Category:   Lisp Libraries
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(defmacro require-when (module &rest situations)
  `(eval-when ,(or situations '(:execute :compile-toplevel :load-toplevel))
     (require ,(format nil "ansify/~A" module))))

(require-when "symbol-macrolet" :execute :compile-toplevel)
(require-when "setf")
(require-when "typespec+")

(provide "ansify")
;;; ansify.l ends here.
