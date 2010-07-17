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
  "SYMBOLS get SHADOWING-IMPORT'ed into the package using CL."
  (let ((symbols (cond ((listp symbols) symbols)
                       ((symbolp symbols) (list symbols))
                       (t (error 'type-error :datum symbols
                                 :expected-type '(or list symbol))))))
    (shadow symbols)
    (setf *shadowing-exported-symbols*
          (append *shadowing-exported-symbols* symbols))
    (dolist (pkg (package-used-by-list :common-lisp))
      (shadowing-import symbols pkg))))

(defun install (&optional (package *package*))
  "Install CL into PACKAGE."
  (unless (member package (package-use-list package))
    (use-package :common-lisp package))
  (shadowing-import *shadowing-exported-symbols* package))

;;; common-lisp/package.l ends here.
