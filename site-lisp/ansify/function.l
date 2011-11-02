;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/function.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; References:
;;
;; [CLHS: Issue FUNCTION-NAME Writeup][1]
;;   [1]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss174_w.htm

;;; Commentary
;;
;; Issue FUNCTION-NAME highlight
;; =============================
;;
;; FUNCTION-NAME:SMALL
;; -------------------
;; - Introduce new concept "function name", which is a symbol or a list
;;   `(setf READER)`
;; - Add function `fdefinition` and `setf` of it
;; - Function `fboundp`, `fmakunbound`, and special operator `function`
;; - Macro `setf` expansion rule: expands into a call to setf function
;;
;; FUNCTION-NAME:MEDIUM
;; --------------------
;; - Macro `defun`
;;
;; FUNCTION-NAME:LARGE
;; -------------------
;; 1. Function `compile`
;; 2. Function `disassemble`
;; 3. Declaration `ftype`, `inline`, and `notinline`
;; 4. Special form `flet` and `labels`
;; 5. Macro `trace` and `untrace`
;; 6. Function `ed`
;; 7. Syntax for function call
;; 8. Macro `defmacro`, `macrolet`, and `macro-function` (removed)
;; 9. Add optional environment argument to Function `fdefinition`, `fboundp`,
;;    `fmakunbound`, and setf for `fdefinition` (removed)
;; 10. Macro `setf` expansion rule: support local setf functions
;; 11. (documentation thing)

;; Conformance Note
;; ================
;; Currently, Proposal FUNCTION-NAME:MEDIUM is implemented with following
;; exceptions:
;; - Special form `function`: doesn't accept list form of function-name.
;; - Macro `setf` expansion rule: expands into call for setf function only
;;   when corresponding setf function is defined.
;;   The reason for this is that original implementation of `setf` doesn't
;;   expand such a way, and I left it untouched. Instead defining setf
;;   function via `defun` also defines setf expander for READER, so it is
;;   required to define setf functions at compile-time to compile corresponding
;;   setf forms correctly.

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/config")
  (require "ansify/condition")
  (require "ansify/autoload-defs")
  (require "ansify/restart"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(defun
                      fdefinition
                      ;function-lambda-expression
                      fboundp
                      fmakunbound
                      )))

(lisp:defun setf-function-name-p (x)
  (and (consp x)
       (eql (first x) 'setf)
       (symbolp (second x))
       (null (cddr x))))

(lisp:defun get-setf-function-symbol (name)
  (unless (setf-function-name-p name)
    (error "ïsê≥Ç»ä÷êîñºÇ≈Ç∑: ~S" name))
  (let ((accessor (second name)))
    (intern (format nil "(setf ~A)" accessor)
            (symbol-package accessor))))

(lisp:defun canonicalize-function-name (name)
  (if (symbolp name) name
    (get-setf-function-symbol name)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro defun (name (&rest lambda-list) &body body)
    (labels ((warn-redefinition (sym name)
               `(when (and (fboundp ',sym)
                           *warn-on-redefine-function*)
                  (warn 'redefining-function :name ',name :type 'function))))
      (let ((lambda-list
              ;; NOTE: xyzzy doesn't recognize `&allow-other-keys` as one
              ;; of lambda-list-keyword. So it tries to use it as normal
              ;; parameter and signals an error, since it is defined as
              ;; constant in ansify. To deal with this, simply remove it
              ;; from LAMBDA-LIST.
              ;; Note that xyzzy allows unspecified keys without explicitly
              ;; allowed by `&allow-other-keys` or alike.
              (remove &allow-other-keys lambda-list)))
        (cond ((symbolp name)
               `(progn
                  ,(warn-redefinition name name)
                  (lisp:defun ,name ,lambda-list ,@body)))
              ((consp name)
               (let ((sym (get-setf-function-symbol name)))
                 `(progn
                    ,(warn-redefinition sym name)
                    (lisp:defun ,sym ,lambda-list ,@body)
                    (defsetf ,(second name) ,(cdr lambda-list) (,(car lambda-list))
                      (list ',sym ,@lambda-list))))))))))

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm

(lisp:defun fdefinition (function-name)
  (symbol-function (canonicalize-function-name function-name)))

(defsetf fdefinition (function-name) (new-definition)
  (let (($name (gensym "name-")))
    `(let ((,$name (canonicalize-function-name ,function-name)))
       (if (or (functionp ,new-definition)
               (si:*builtin-function-p ,new-definition))
         (setf (symbol-function ,$name) ,new-definition)
         (error 'type-error :datum ,new-definition :expected-type 'function)))))

(lisp:defun fboundp (function-name)
  (lisp:fboundp (canonicalize-function-name function-name)))

(lisp:defun fmakunbound (function-name)
  (cond ((setf-function-name-p function-name)
         (let ((name (canonicalize-function-name function-name)))
           (lisp:fmakunbound name)
           (remprop (second function-name) 'lisp::setf-lambda)
           function-name))
        ((symbolp function-name)
         (lisp:fmakunbound function-name))
        (t
         (error 'type-error :datum function-name
                :expected-type '(or symbol (cons (eql setf) (cons symbol nil)))))))


(provide "ansify/function")
;;; ansify/function.l ends here.
