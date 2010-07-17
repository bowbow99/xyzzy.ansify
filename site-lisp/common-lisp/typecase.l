;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/typecase.l
;;;
;;; [CLHS: Macro TYPECASE, CTYPECASE, ETYPECASE]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Body/m_tpcase.htm
;;;

(require "common-lisp/package")
(require "condition-restart")

(in-package :common-lisp)

(export '(typecase
          etypecase
          ctypecase
          otherwise))

(defun %otherwise-clause-p (clause)
  (member (car clause) '(t otherwise)))

(defmacro typecase (key &body clauses)
  (let (($key (gensym)))
    `(let ((,$key ,key))
       (cond
        ,@(mapcar (lambda (clause)
                    (if (%otherwise-clause-p clause)
                        `(t ,@(cdr clause))
                      `((typep ,$key ',(car clause)) ,@(cdr clause))))
            clauses)))))

(defmacro etypecase (key &body clauses)
  (let (($key (gensym)))
    `(let ((,$key ,key))
       (cond
        ,@(mapcar (lambda (clause)
                    `((typep ,$key ',(car clause)) ,@(cdr clause)))
            clauses)
        (t (error 'type-error :datum ,$key
                  :expected-type '(or ,@(mapcar #'car clauses))))))))

(defmacro ctypecase (place &rest clauses)
  (let (($key (gensym)))
    `(block ctypecase
       (tagbody #1=#:ctypecase
         (let ((,$key ,place))
           (return-from ctypecase
             (cond
              ,@(mapcar (lambda (clause)
                          `((typep ,$key ',(car clause)) ,@(cdr clause)))
                  clauses)
              (t (restart-case
                     (error 'type-error :datum ,$key
                            :expected-type '(or ,@(mapcar #'car clauses)))
                   (store-value (value)
                     :report (lambda (s)
                               (format s "`~S' ÇÃílÇïœçXÇ∑ÇÈ" ',place))
                     :interactive (lambda ()
                                    (list (restart::read-value-for ',place)))
                     (setf ,place value)))
                 (go #1#)))))))))

#+xyzzy
(setf (get 'typecase 'ed:lisp-indent-hook) 1
      (get 'etypecase 'ed:lisp-indent-hook) 1
      (get 'ctypecase 'ed:lisp-indent-hook) 1)

;;; common-lisp/typecase.l ends here.
