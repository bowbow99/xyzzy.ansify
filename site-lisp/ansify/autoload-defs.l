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

;; symbol-macrolet.l
(%autoload symbol-macrolet "symbol-macrolet" nil t)
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1)

;; destructuring-bind.l
(%autoload destructuring-bind "destructuring-bind" nil t)
(setf (get 'destructuring-bind 'ed:lisp-indent-hook) 2)

(shadowing-export '(&allow-other-keys))
(defconstant &allow-other-keys '&allow-other-keys)

;; assertion.l
(%autoload assert "assertion" nil t)
(%autoload check-type "assertion" nil t)

;; typecase.l
(%autoload typecase  "typecase" nil t)
(%autoload etypecase "typecase" nil t)
(%autoload ctypecase "typecase" nil t)
(%autoload case  "typecase" nil t)
(%autoload ecase "typecase" nil t)
(%autoload ccase "typecase" nil t)

(dolist (sym '(typecase etypecase ctypecase case ecase ccase))
  (setf (get sym 'ed:lisp-indent-hook) 1))


(provide "ansify/autoload-defs")
;;; ansify/autoload-defs.l ends here.
