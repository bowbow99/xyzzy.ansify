;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/condition.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(shadowing-export '(#:style-warning
                    #:print-not-readable
                    #:print-not-readable-object
                    ))


;;;; local utilities

(defmacro define-simple-condition (parent)
  `(define-condition ,(mksym "simple-~A" parent) (,parent)
     (format-control format-arguments)
     (:report (lambda (c stream)
                (apply #'format stream
                       (si:*slot-value c :format-control)
                       (si:*slot-value c :format-arguments))))))

(eval-when (:execute :compile-toplevel)
  (defun condition-signal-method (condition)
    "Return appropriate signalling operator for CONDITION."
    (labels ((sub-condition-p (x parent)
               (si:*structure-subtypep
                (get x 'si:structure-definition)
                (get parent 'si:structure-definition))))
      (cond ((sub-condition-p condition 'serious-condition) 'error)
            ((sub-condition-p condition 'warning) 'warn)
            ((sub-condition-p condition 'condition) 'signal)
            (t (error 'type-error :datum condition
                      :expected 'condition-type))))))

(defmacro define-signaller (condition)
  "Define a signaller function which takes DATUM and ARGS as standard
signallers and signal `simple-CONDITION` if DATUM is a string or type
specified by DATUM.
Note that you need to define CONDITION and `simple-CONDITION` first."
  (let ((signal-method (condition-signal-method condition)))
    `(defun ,(mksym "signal-~A" condition) (datum &rest args)
       ,(format nil "Signal a condition of type `~S`." condition)
       (if (stringp datum)
         (,signal-method ',(mksym "simple-~A" condition)
          :format-control datum :format-arguments args)
         (apply #',signal-method datum args)))))


;;;; missing conditions

;;;;; style-warning

(define-condition style-warning (warning) ()
  (:report (lambda (sw stream)
             (format stream "(�A�E)��Ų"))))

(define-simple-condition style-warning)
(define-signaller style-warning)

;;;;; print-not-readable

(define-condition print-not-readable (error)
  (object)
  (:report (lambda (x stream)
             (format stream "�ǂݍ��݉\�Ȍ`���ŏo�͂ł��܂���: ~S"
                   (print-not-readable-object x)))))


;;;; ansify specific

(define-condition undefined-type-specifier (warning) (datum)
  (:report (lambda (x stream)
             (format stream "����`�̌^�w��q�ł�: ~S"
                   (undefined-type-specifier-datum x)))))

(define-condition invalid-type-specifier (warning) (datum)
  (:report (lambda (x stream)
             (format stream "�ׂ������Ƃ������Ό^�w��q `~S` �͕s���ł�"
                   (invalid-type-specifier-datum x)))))

(define-condition otherwise-in-middle (warning) ()
  (:report (lambda (x stream)
             (format stream "case ���̓r���� otherwise �߂�����܂�"))))

(define-condition redefining-function (warning) (name type)
  (:report (lambda (x stream)
             (format stream "~[�֐�~;�}�N��~] `~S` ���Ē�`���Ă��܂�"
                   (position (redefining-function-type x) '(function macro))
                   (redefining-function-name x)))))


(provide "ansify/condition")
;;; ansify/condition.l ends here.