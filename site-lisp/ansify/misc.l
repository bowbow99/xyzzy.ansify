;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/misc.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

;; NOTE: Used in expansion of `destructuring-bind`
(defun %assert-keyword-list (keyword-list allowed-keywords allow-other-keys)
  "Check whether KEYWORD-LIST is proper or not, and return it as-is."
  ;; NOTE: `&allow-othe-keys X` is always permitted
  (setq allowed-keywords (cons '&allow-other-keys allowed-keywords))
  (unless (and (proper-list-p keyword-list)
               (evenp (length keyword-list))
               (or allow-other-keys
                   (getf keyword-list :allow-other-keys)
                   (do ((list keyword-list (cddr list)))
                       ((null list) t)
                     (unless (member (car list) allowed-keywords)
                       (return nil)))))
    (error 'invalid-keyword-list :datum keyword-list))
  keyword-list)

(provide "ansify/misc")
;;; ansify/misc.l ends here.
