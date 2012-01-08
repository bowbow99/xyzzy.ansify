;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify.l --- ANSI Common Lisp Ç…Ç†Ç¡Çƒ xyzzy Ç…Ç»Ç¢Ç‡ÇÃãlÇﬂçáÇÌÇπ
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.03.00
;;; Category:   Lisp Libraries
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(defconstant +version+ "0.03.00")

(require "ansify/config")
(require "ansify/autoload-defs")
(require "ansify/function")
(require "ansify/setf")
(require "ansify/typespec+")
(require "ansify/restart")
(require "ansify/compile")
(require "ansify/misc")

(provide "ansify")
;;; ansify.l ends here.
