;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/base/package.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:


;;;; Package Definitions

(defpackage :ansify.util
  (:use :lisp))

(defpackage :ansify.ext)

(defpackage :ansify
  (:use :lisp :ansify.util :ansify.ext))

(in-package :ansify)


;;;; Shadowing Export

(defvar *shadowing-exported-symbols* nil
  "List of symbols to be `shadowing-import`ed when ansify is installed.")

(defvar *installed-packages* nil
  "List of packages those ansify is installed.")

(defun shadowing-export (symbols)
  "SYMBOLS will be SHADOWING-IMPORT'ed when ansify is installed into a package."
  (labels ((string-designator-p (x)
             ;; NOTE: To be precise, character can be a string designator.
             (or (symbolp x) (stringp x))))
    (let* ((package (find-package :ansify))
           (symbols (mapcar (lambda (name)
                              (shadow (string name) :ansify)
                              (intern (string name) :ansify))
                      (cond ((and (listp symbols)
                                  (every #'string-designator-p symbols))
                             symbols)
                            ((string-designator-p symbols)
                             (list symbols))
                            (t
                             (error 'type-error
                                    :expected-type (or list string symbol)
                                    :datum symbols))))))
      (setf *shadowing-exported-symbols*
            (union *shadowing-exported-symbols* symbols))
      (dolist (pkg *installed-packages*)
        (dolist (sym symbols)
          (unless (eql (find-symbol (string sym) pkg) sym)
            (shadowing-import sym pkg))))
      (export symbols package))))

(defun install (&optional (package *package*))
  "Install ansify into PACKAGE."
  (pushnew package *installed-packages*)
  (shadowing-import *shadowing-exported-symbols* package)
  t)

(provide "ansify/base/package")
;;; ansify/base/package.l ends here.
