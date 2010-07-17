;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/typecase.l
;;;
;;; [CLHS: Macro TYPECASE, CTYPECASE, ETYPECASE]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Body/m_tpcase.htm
;;;

(require "common-lisp/package")

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

;;; common-lisp/typecase.l ends here.
