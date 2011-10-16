;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/assertion.l
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/restart")
  (require "ansify/typespec+"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:assert
                      #:check-type
                      )))


;;;; Assertions

(defmacro assert (test-form &optional places (datum t datum-supplied-p) &rest args)
  "Ensure TEST-FORM evaluates to non-nil."
  `(tagbody
     #1=#:assertion-root
     (unless ,test-form
       (restart-case
           (error ,@(if datum-supplied-p
                      (list* datum args)
                      `("assert に失敗しました: ~S" ',test-form)))
         (continue (&rest new-values)
           :report (lambda (stream)
                     (format stream "~:[やり直す。~;~@*値を変更してやり直す。(~{~S~^ ~})~]"
                       ',places))
           :interactive (lambda ()
                          (mapcar (lambda (place)
                                    (read-value
                                     (format nil "`~S' の値: " place)))
                            ',places))
           (setf ,@(let ((i -1))
                     (mapcan (lambda (place)
                               `(,place (nth ,(incf i) new-values)))
                       places)))
           (go #1#))))))

(defmacro check-type (place typespec &optional string)
  "Ensure value of PLACE is type of TYPESPEC."
  `(tagbody
     #1=#:check-type-root
     (let ((#2=#:datum ,place))
       (unless ,(optimize-type-check '#2# typespec)
         (restart-case
             (error 'si::check-type-error
               :place ',place
               :datum #2#
               :expected-type ',typespec
               :string ,string)
           (store-value (new-value)
             :report (lambda (stream)
                       (format stream "`~S' の値を変更してやり直す。" ',place))
             :interactive (lambda ()
                            (list (read-value
                                   (format nil "`~S' の値: " ',place)
                                   (typepred ,typespec))))
             (setf ,place new-value)
             (go #1#)))))))


(provide "ansify/assertion")
;;; ansify/assertion.l ends here.
