;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/autoload-defs.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(defmacro %autoload (symbol module &optional command-p macro-p)
  `(progn
     (shadowing-export '(,symbol))
     (autoload (find-symbol (string ',symbol) :ansify)
               ,(format nil "ansify/~A" module)
               ,command-p ,macro-p)))

;; symbol-macrolet
(%autoload symbol-macrolet "symbol-macrolet" nil t)
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1)

;; destructuring-bind
(%autoload destructuring-bind "destructuring-bind" nil t)
(setf (get 'destructuring-bind 'ed:lisp-indent-hook) 2)

(provide "ansify/autoload-defs")
;;; ansify/autoload-defs.l ends here.
