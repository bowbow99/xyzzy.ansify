;;; -*- mode: lisp -*-
;;;
;;; common-lisp/package.l
;;;

;;;
;;; * Package Definitions

(defpackage :common-lisp
  (:use :lisp)
  (:nicknames "COMMON-LISP" "CL" "cl"))

(defpackage :common-lisp-user
  (:use :lisp :common-lisp)
  (:nicknames "COMMON-LISP-USER" "CL-USER" "cl-user"))

;;; common-lisp/package.l ends here.
