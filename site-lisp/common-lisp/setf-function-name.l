;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; setf-function-name.l
;;;
;;; [CLHS: Issue FUNCTION-NAME Writeup]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Issues/iss174_w.htm
;;;

(require "common-lisp/package")

(in-package :common-lisp)

(export '(fdefinition))

(shadowing-export '(fboundp
                    fmakunbound))


(defun %setf-function-name-p (name)
  (and (consp name)
       (eq (car name) 'setf)
       (and (cadr name)
            (symbolp (cadr name)))
       (null (cddr name))))

(defun %function-symbol (name)
  (cond
   ((symbolp name) name)
   ((%setf-function-name-p name)
    (intern (format nil "~A" name) (symbol-package (cadr name))))
   (t (error 'type-error :datum name
             :expected-type '(or symbol cons)))))

;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fdefin.htm
(defun fdefinition (name)
  (symbol-function (%function-symbol name)))

(defun %set-fdefinition (name new-definition)
  (setf (symbol-function (%function-symbol name)) new-definition)
  new-definition)

(defsetf fdefinition %set-fdefinition)

;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fbound.htm
(defun fboundp (name)
  (lisp:fboundp (%function-symbol name)))

;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fmakun.htm
(defun fmakunbound (name)
  (lisp:fmakunbound (%function-symbol name))
  name)



(provide "common-lisp/setf-function-name")

;;; setf-function-name.l ends here.
