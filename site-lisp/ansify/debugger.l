;;; -*- mode: lisp; package: ansify.debugger -*-
;;;
;;; ansify/debugger.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/restart")
  (require "ansify/symbol-macrolet")
  (require "ansify/typecase"))


;;;; package

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(invoke-debugger
                      break
                      *debugger-hook*)))

(defpackage :ansify.debugger
  (:use :lisp :editor)
  (:import-from :ansify.util
    #:set-indent #:aif #:awhen #:awhile))

(in-package :ansify.debugger)

(export '(*default-debugger*
          *get-backtrace-function*
          *find-toplevel-abort-function*
          ;; for debugger implementation
          with-debug-session
          ssn-condition
          ssn-restarts
          ssn-backtrace
          ))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (ansify::install))


;;;; debugger customization

(defparameter *get-backtrace-function* nil
  "�f�o�b�K���N�����ꂽ���Ƀo�b�N�g���[�X���擾����֐����w�肵�܂��B
�֐��̓f�o�b�K���N�������R���f�B�V�������󂯎��A���̃R���f�B�V������
�ʒm�Ɏ��������s�̃o�b�N�g���[�X��Ԃ��Ȃ���΂Ȃ�܂���B
�o�b�N�g���[�X�̌`���͌��݂̂Ƃ��땶����̃��X�g�ł����A�����ύX�����
�\��ł��B
�擾�ł��Ȃ��ꍇ�� nil ��Ԃ������ł��܂��B")

(defparameter *default-debugger* nil
  "�f�o�b�K���N������֐����w�肵�܂��B")


;;;; debug session

(defstruct session
  (condition nil
    :type condition)
  (restarts nil
    :type list)
  (backtrace nil
    :type list))

(defparameter *debug-sessions* nil
  "Holds list of active debug sessions.
This is being a list to allow enter debugger recursively.")

(defmacro with-debug-session (&body body)
  `(progn
     (unless *debug-sessions*
       (error "Not in a debug session."))
     (symbol-macrolet ((ssn-condition (session-condition (car *debug-sessions*)))
                       (ssn-restarts  (session-restarts  (car *debug-sessions*)))
                       (ssn-backtrace (session-backtrace (car *debug-sessions*))))
       ,@body)))
(set-indent 'with-debug-session 0)

(defun create-debug-session (condition)
  (make-session :condition condition
                :restarts (compute-restarts condition)
                :backtrace (awhen *get-backtrace-function*
                             (funcall *get-backtrace-function* condition))))


;;;; ANSI debugger related APIs

(defvar *debugger-hook* nil
  "*�f�o�b�K�ɓ���O�Ɏ��s�����t�b�N�֐����w�肵�܂��B
�t�b�N�֐��̓f�o�b�K���N�������R���f�B�V������ *debugger-hook* �ɐݒ�
����Ă����l�i���̃t�b�N�֐����Ă΂�鎞�ɂ� *debugger-hook* �� nil ��
�ݒ肳��܂��B�j��^�����܂��B")

(defun invoke-debugger (condition)
  "�f�o�b�K���N�����܂��B"
  (let ((*debug-sessions* (cons (create-debug-session condition)
                                *debug-sessions*)))
    (when *debugger-hook*
      (let ((hook *debugger-hook*)
            (*debug-sessions* nil))
        (funcall hook condition hook)))
    (funcall *default-debugger* condition)
    ;; FIXME: What if debugger didn't transfer control?
    ))

(define-condition break (condition)
  (format-string format-arguments)
  (:report (lambda (x out)
             (apply #'format out
                    (break-format-string x)
                    (break-format-arguments x)))))

(defun break (format-control &rest format-arguments)
  "Lisp �v���O�����̎��s���ꎞ��~���ăf�o�b�K���N�����܂��B"
  (restart-case
      (let ((*debugger-hook* nil)
            (break (make-condition 'break
                                   :format-string format-control
                                   :format-arguments format-arguments)))
        (invoke-debugger break))
    (continue ()
      :report "break �𔲂��đ��s�B")))


;;;; simple (damb) debugger

;;;;; backtrace from *Trace Output* buffer

(defun last-backtrace-from-buffer (buffer condition)
  (save-excursion
    (with-set-buffer
      (set-buffer buffer)
      (goto-char (point-max))
      (when (or (null condition)
                (looking-back (format nil "~A~2%" condition)))
        (skip-white-backward)
        (goto-bol)
        (let ((end (point)))
          (forward-line -1)
          (while (looking-at "^>CALL STACK")
            (unless (forward-line -1)
              (if (= (point) (point-min))
                (return)
                (return-from last-backtrace-from-buffer nil))))
          (split-string (buffer-substring (point) end)
                        #\LFD))))))

(defun backtrace-from-trace-output-buffer (condition)
  (awhen (and si:*trace-on-error*
              (find-buffer "*Trace Output*"))
    (last-backtrace-from-buffer it condition)))

(unless *get-backtrace-function*
  (setf *get-backtrace-function* 'backtrace-from-trace-output-buffer))

;;;;; keymap

(defvar *simple-debugger-keymap* nil)
(unless *simple-debugger-keymap*
  (setf *simple-debugger-keymap* (make-sparse-keymap))
  (macrolet ((define-key-bindings (key command &rest more)
               `(progn
                  (define-key *simple-debugger-keymap* ,key ',command)
                  ,(when more `(define-key-bindings ,@more)))))
    (define-key-bindings
     #\r invoke-restart-by-index
     #\a invoke-abort
     #\q invoke-abort
     #\c invoke-continue)))

;;;;; commands

(defun invoke-abort ()
  "���s�𒆎~���ăg�b�v���x���܂Ŗ߂�܂��B"
  (interactive)
  (with-debug-session
   (abort ssn-condition)))

(defun invoke-restart-by-index (n)
  "N�Ԗڂ̍ċN�����N�����܂��B"
  (interactive "Nrestart: ")
  (with-debug-session
    (let ((restart (or (nth n ssn-restarts)
                       (error 'range-error :datum n))))
      (invoke-restart-interactively restart))))

(defun invoke-continue ()
  "continue �ċN��������΋N�����܂��B"
  (interactive)
  (with-debug-session
    (aif (find-restart 'continue ssn-condition)
      (invoke-restart-interactively it)
      (message "continue �ċN���͖������ۂ�"))))

;;;;; debugger UI

(defun get-debugger-buffer ()
  (let ((buffer (get-buffer-create "*Debug*")))
    (setup-temp-buffer buffer)
    (erase-buffer buffer)
    buffer))

(defun exhibit-debug-session (buffer)
  (with-output-to-buffer (buffer (buffer-size buffer))
    (with-debug-session
      (format t "Debugger entered on `~S`:~%~{  ~A~%~}~%"
            (si:*structure-definition-name (si:*structure-definition ssn-condition))
            (split-string (si:*condition-string ssn-condition) #\LFD))
      (progn
        (princ "Restarts:\n")
        (do ((i 0 (1+ i))
             (rest ssn-restarts (cdr rest)))
            ((null rest))
          (let ((restart (car rest)))
            (format t "  ~2D: [~10@A] ~A~%" i (restart-name restart) restart))))
      (format t "~&~%Backtrace:~%~{  ~A~%~}"
            (or ssn-backtrace '("-- not available --"))))))

(defun simple-debugger (condition)
  (let ((buffer (get-debugger-buffer))
        (obuf (selected-buffer)))
    ;; open and setup debugger buffer
    (progn
      (exhibit-debug-session buffer)
      (set-buffer buffer)
      (refresh-screen))
    ;; casual command-loop
    (unwind-protect
        (let (key command)
          (while (setf key (read-char *keyboard*))
            (if (setf command (lookup-keymap *simple-debugger-keymap* key))
              (handler-case
                  (call-interactively command)
                (error (e) (si:*print-condition e)))
              (message "debugger: no command bound to ~A." key))
            (refresh-screen)))
      (set-buffer obuf)
      (delete-buffer buffer))))

(unless *default-debugger*
  (setf *default-debugger* 'simple-debugger))


(provide "ansify/debugger")
;;; ansify/debugger.l ends here.