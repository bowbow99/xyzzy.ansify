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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:print-unreadable-object
                      )))


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

(defmacro print-unreadable-object ((object stream &key type identity)
                                   &body body)
  (let (($object (gensym "$object-"))
        ($stream (gensym "$stream-"))
        ($type   (gensym "$type-"))
        ($identity (gensym "$identity-")))
    `(let ((,$object ,object)
           (,$stream ,stream)
           (,$type   ,type)
           (,$identity ,identity))
       (princ "#<" ,$stream)
       (when ,$type
         (let ((,$type (type-of ,$object)))
           (prin1 (cond ((eql ,$type 'lisp:structure)
                         (si:*structure-definition-name
                          (si:*structure-definition ,$object)))
                        (t ,$type))
                  ,$stream))
         ,(when (or body identity) `(princ #\SPC ,$stream)))
       ,@body
       (when ,$identity
         ,(when body `(princ #\SPC ,$stream))
         (prin1 (si:address-of ,$object)))
       (princ ">" ,$stream)
       nil)))

(setf (get 'print-unreadable-object 'ed:lisp-indent-hook) 1)

(provide "ansify/misc")
;;; ansify/misc.l ends here.
