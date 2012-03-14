;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/profile.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/restart")
  (require "ansify/misc"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(time
                      trace
                      untrace
                      )))

(in-package :ansify.ext)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(*use-performance-counter*
            ;; deterministic profile
            profile unprofile profile-reset profile-report
            )))

(defpackage :ansify.profile
  (:use :lisp :editor)
  (:import-from :ansify.ext
    #:*use-performance-counter*
    #:profile #:unprofile #:profile-reset #:profile-report))

(in-package :ansify.profile)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (ansify::install))


;;;; tick

;;;;; performance-counter

(defun pc-elapsed (start end &optional (unit :msec))
  (* (- end start)
     (/ si:*performance-counter-frequency*)
     (case unit
       (:sec 1)
       (:msec 1000)
       (:usec 1000000))))


(setf (symbol-function 'tick) #'si:performance-counter
      (symbol-function 'elapsed) #'pc-elapsed)

;; TODO: implemente me, plz.
(defun toggle-performance-counter-enabled-p (&optional (flag nil sv))
  )




;;;; Macro time
;;
;; http://www.lispworks.com/documentation/lw51/CLHS/Body/m_time.htm

(defmacro time (form)
  `(let ((#1=#:start (tick)))
     (multiple-value-prog1
         ,form
       (format t "~&Elapsed: ~,6F msec~%" (elapsed #1# (tick))))))


;;;; Profile specified functions

;; FIXME: don't create hash-table at load time if loaded in siteinit.l.
(defvar *profile-data-table* (make-hash-table))

(defun list-profiled-functions ()
  (let ((functions '()))
    (maphash (lambda (fn data)
               (push fn functions))
             *profile-data-table*)
    functions))

(defun function-profiled-p (function-name)
  (nth-value 1 (gethash function-name *profile-data-table*)))

(defun profile-reset ()
  "これまでに溜まったプロファイルのデータをクリアします。"
  (dolist (function-name (list-profiled-functions))
    (setf (gethash function-name *profile-data-table*) nil))
  t)

(defparameter *profiled-apply-count* 0)

(defun enprofile-0 (function-name closure)
  (lambda (&rest args)
    (let ((start (tick))
          (ocount *profiled-apply-count*)
          (*profiled-apply-count* (1+ *profiled-apply-count*)))
      (multiple-value-prog1
          (apply closure args)
        (push (cons (elapsed start (tick))
                    (- *profiled-apply-count* ocount))
              (gethash function-name *profile-data-table*))))))

(defun enprofile-1 (function-name)
  (unless (symbolp function-name)
    (error 'type-error :datum function-name :expected-type 'symbol))
  (unless (fboundp function-name)
    (warn "profile - 未定義の関数をプロファイルすんな: ~S" function-name)
    (return-from enprofile-1))
  (unless (functionp (symbol-function function-name))
    (warn "profile - 関数じゃないものはプロファイルできねっす: ~S" function-name)
    (return-from enprofile-1))
  (when (function-profiled-p function-name)
    (unprofile function-name))
  (let ((thunk (symbol-function function-name)))
    (setf (get function-name 'original-function) thunk
          (gethash function-name *profile-data-table*) nil
          (symbol-function function-name) (enprofile-0 function-name thunk))))

(defun unprofile-1 (function-name)
  (unless (symbolp function-name)
    (error 'type-error :datum function-name :expected-type 'symbol))
  (unless (function-profiled-p function-name)
    (warn "unprofile - 元からプロファイルしてないです: ~S" function-name))
  (progn
    (remhash function-name *profile-data-table*)
    (setf (symbol-function function-name)
          (get function-name 'original-function))
    function-name))

(defun profile (&rest names)
  "NAMES に指定した関数（複数可）をプロファイルするようにします。
関数をプロファイルしておくと、普通に xyzzy を動かしている間にその関数が
実行されると掛かった時間を記録しておき（その分実際の処理時間は遅くなり
ます。）、後から profile-report で集計結果を見ることができます。"
  (dolist (name names)
    (enprofile-1 name))
  (list-profiled-functions))

(defun unprofile (&rest names)
  "NAMES に指定した関数（複数可）のプロファイルを解除します。"
  (dolist (name (or names (list-profiled-functions)))
    (unprofile-1 name)))

;; TODO: profile-package, profile-regexp


(defun measure-profile-overhead (&optional (n 30000))
  (let ((example (gensym "example-")))
    (setf (symbol-function example) (constantly t))
    ;(gc)
    (message "profile: measuring overhead...")
    (macrolet ((measure (form)
                 `(let ((#1=#:start (tick)))
                    (dotimes (#2=#:i n) ,form)
                    (elapsed #1# (tick)))))
      (prog1
          (let ((*profile-data-table* (make-hash-table)))
            (/ (- (progn
                    (profile example)
                    (measure (funcall example)))
                  (progn
                    (unprofile example)
                    (measure (funcall example))))
               n))
        (message "profile: measuring overhead... done")))))

(defun analyze-profile-data (&key (table *profile-data-table*)
                                  (overhead (measure-profile-overhead)))
  (message "profile: analyzing profile data...")
  (let ((results '()))
    (maphash (lambda (function-name data)
               (when data
                 (message "profile: analyzing profile data...~S" function-name)
                 (let* ((ncall (length data))
                        (adjusted (mapcar (lambda (datum)  ; datum = (time . count)
                                            (- (car datum) (* overhead (cdr datum))))
                                        data))
                        (total (apply #'+ adjusted))
                        (per-call (/ total ncall)))
                   (push (list ncall total per-call function-name)
                         results))))
             table)
    (message "profile: analyzing profile data...done.")
    (sort results #'> :key 'second)))

(defun profile-report ()
  (let ((data (analyze-profile-data)))
    (format t "~7@A ~9@A ~9@A  ~A~%" :#call :total :per-call "function name")
    (format t "~:{~7@A ~9,6F ~9,6F  ~S~%~}" data)))



;;;; Macro trace, untrace
;;
;; http://www.lispworks.com/documentation/lw50/CLHS/Body/m_tracec.htm


(defun trace-1 (function-name)
  )


(defmacro trace (names))

(defmacro untrace (names))


(provide "ansify/profile")
;;; ansify/profile.l ends here.
