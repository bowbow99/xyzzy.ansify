;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/package.l
;;;

;;;;
;;;; * Package Definitions

(defpackage :common-lisp
  (:use :lisp)
  (:nicknames "COMMON-LISP" "CL" "cl"))

(defpackage :common-lisp-user
  (:use :lisp :common-lisp)
  (:nicknames "COMMON-LISP-USER" "CL-USER" "cl-user"))


(in-package :common-lisp)

;;;;
;;;; * Shadowing-export

(defvar *shadowing-exported-symbols* nil)

(defun shadowing-export (symbols)
  (shadow (mapcar #'string symbols) :common-lisp)
  (let ((syms (let ((*package* (find-package :common-lisp)))
                (mapcar (lambda (x) (intern (string x)))
                        symbols))))
    (dolist (sym syms)
      (pushnew sym *shadowing-exported-symbols*))
    (dolist (pkg (package-used-by-list :common-lisp))
      (shadowing-import syms pkg))))

(defun install (&optional (package *package*))
  "Install CL into PACKAGE."
  (unless (member package (package-use-list package))
    (use-package :common-lisp package))
  (shadowing-import *shadowing-exported-symbols* package))

(provide "common-lisp/package")

;;; common-lisp/package.l ends here.
