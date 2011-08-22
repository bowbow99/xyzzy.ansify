;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/base/package.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;

;;; Code:

;;;;
;;;; * Package Definitions

(defpackage :ansify
  (:use :lisp))

(in-package :ansify)


;;;;
;;;; * Shadowing Export

(defvar *shadowing-exported-symbols* nil)

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
      (dolist (pkg (package-used-by-list :ansify))
        (dolist (sym symbols)
          (unless (eql (find-symbol (string sym) pkg) sym)
            (shadowing-import sym pkg))))
      symbols)))

(defun install (&optional (package *package*))
  "Install ansify into PACKAGE."
  (unless (member package (package-use-list package))
    (use-package :ansify package))
  (shadowing-import *shadowing-exported-symbols* package)
  t)

;;;;
;;;; * Exporting symbols

(eval-when (:execute :compile-toplevel :load-toplevel)

;;; NOTE: Most of following are SHADOWING-EXPORT'ed because they conflict with
;;; former versions of libraries which is still in use in the environment I'm
;;; using. Should be just EXPORT'ed when I got rid of them.

;;; NOTE: Some of exported symbols should be AUTOLOAD'ed.

;; symbol-macrolet.l
(shadowing-export '(symbol-macrolet))

;; setf.l
(shadowing-export '(get-setf-expansion
                    define-setf-expander))

;; typespec+.l
(shadowing-export '(typep))

;; restart.l
(shadowing-export '(;; Restart establishing macros
                    restart-case
                    restart-bind
                    with-simple-restart
                    with-condition-restarts
                    ;; Restart manipulating functions
                    restart-name
                    compute-restarts
                    find-restart
                    invoke-restart
                    invoke-restart-interactively
                    ;; Condition Signaller
                    cerror
                    warn
                    ;; Standard restart names
                    abort
                    continue
                    muffle-warning
                    use-value
                    store-value
                    ;; as a typespec
                    restart))

(export (list (intern "read-value" :editor)) :editor)

;; assertion.l
(shadowing-export '(assert
                    check-type))

;; typecase.l
(shadowing-export '(typecase
                    etypecase
                    ctypecase
                    otherwise))

;; destructuring-bind.l
(shadowing-export '(destructuring-bind
                    &allow-other-keys))

) ; end of `eval-when`


(provide "ansify/base/package")
;;; ansify/base/package.l ends here.
