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
  (require "ansify/base")
  (require "ansify/condition"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:print-unreadable-object #:constantly #:type-of
                      #:the #:nth-value #:special-operator-p)))


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

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm
(defun type-of (object)
  ;; NOTE: type-of  - original one return just `structure`
  (if (si:*structurep object)
    (si:*structure-definition-name (si:*structure-definition object))
    (lisp:type-of object)))

;; http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm
(defmacro print-unreadable-object ((object stream &key type identity)
                                   &body body)
  ;; FIXME: print-unreadable-object  - signal `print-not-readable` if `*print-readably* is true
  (let (($object (gensym "$object-"))
        ($stream (gensym "$stream-"))
        ($type   (gensym "$type-"))
        ($identity (gensym "$identity-")))
    `(let ((,$object ,object)
           (,$stream ,stream)
           (,$type   ,type)
           (,$identity ,identity))
       (when *print-readably*
         (error 'print-not-readable :object ,$object))
       (princ "#<" ,$stream)
       (when ,$type
         (prin1 (type-of ,$object) ,$stream)
         ,(when (or body identity) `(princ #\SPC ,$stream)))
       ,@body
       (when ,$identity
         ,(when body `(princ #\SPC ,$stream))
         (prin1 (si:address-of ,$object) ,$stream))
       (princ ">" ,$stream)
       nil)))

(setf (get 'print-unreadable-object 'ed:lisp-indent-hook) 1)

;; http://www.lispworks.com/documentation/HyperSpec/Body/m_nth_va.htm
(defmacro nth-value (n form)
  "Return Nth value of multiple values FORM returned."
  `(car (nthcdr ,n (multiple-value-list ,form))))

;; http://www.lispworks.com/documentation/HyperSpec/Body/s_the.htm
(defmacro the (value-type form)
  "Specify the type of values returned by FORM."
  form)

(si:*fset 'special-operator-p #'special-form-p)

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_cons_1.htm
(defun constantly (value)
  "Return a function which always return VALUE."
  (lambda (&rest #:args) value))

(provide "ansify/misc")
;;; ansify/misc.l ends here.
