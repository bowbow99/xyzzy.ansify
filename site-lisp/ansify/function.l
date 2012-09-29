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
  (require "ansify/restart"))

(eval-when (:execute :compile-toplevel)
  (require "ansify/destructuring-bind"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(defun
                      fdefinition
                      ;function-lambda-expression
                      fboundp
                      fmakunbound
                      trace untrace
                      ))
  (export-from :ansify.ext
               '(#:function-info
                 #:fwrapper #:find-fwrapper #:define-fwrapper #:fwrap #:funwrap
                 #:call-next-function #:*this-function*
                 ))
  )


;;;; Accessor function-info

(defconstant +fn-info-marker+ '#:fn-info)

(lisp:defun %fn-info-table (fn &optional create)
  (let* ((lambda-list (second (si:closure-body fn)))
         (aux-list    (member '&aux lambda-list))
         (table       (dolist (aux aux-list)
                        (when (and (consp aux)
                                   (eq (car aux) +fn-info-marker+))
                          (return (second aux))))))
    (cond (table table)
          (create
            (let* ((table (make-hash-table :test #'eql)))
              (setf (second (si:closure-body fn))
                    (append (copy-list lambda-list)
                            (unless aux-list (list &aux))
                            (list (list +fn-info-marker+ table))))
              table)))))

(lisp:defun function-info (function key)
  (let ((table (%fn-info-table function)))
    (when (hash-table-p table)
      (gethash key table))))

(defsetf function-info (function key) (new-value)
  `(setf (gethash ,key (%fn-info-table ,function t)) ,new-value))


;;;; function name 

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


;;;; fwrapper

(eval-when (:execute :compile-toplevel :load-toplevel)
  (lisp:defun print-fwrapper (fw out depth)
    (declare (ignore depth))
    (format out "#<fwrapper: ~S>" (fwrapper-name fw))))

(defstruct (fwrapper
            (:print-function print-fwrapper))
  name
  documentation
  thunk)

(defparameter *fwrapper-list* nil
  "Global namespace for fwrapper.")

(lisp:defun %store-fwrapper (fwrapper)
  (setf *fwrapper-list*
        (cons fwrapper
              (delete (fwrapper-name fwrapper) *fwrapper-list*
                      :key #'fwrapper-name))))

(lisp:defun find-fwrapper (name)
  "NAME Ç∆Ç¢Ç§ñºëOÇÃ fwrapper Çï‘ÇµÇ‹Ç∑ÅB"
  (cond ((symbolp name)
          (or (find name *fwrapper-list* :key #'fwrapper-name)
              (error "Undefined fwrapper: ~S" name)))
        ((fwrapper-p name) name)
        (t (error 'type-error :datum name :expected-type '(or symbol fwrapper)))))

;;;;; define-fwrapper

(lisp:defun gen-fwrapper-thunk (name fw-lambda-list decls body)
  `(si:*set-function-name
    (lambda (next-fns &rest call-args)
      ,@decls
      (destructuring-bind ,fw-lambda-list call-args
        (labels ((call-next-function (&rest next-args)
                   (apply (first next-fns)
                          (rest next-fns)
                          (or next-args call-args))))
          ,@body)))
    '(fwrapper ,name)))


(defmacro define-fwrapper (name fw-lambda-list &body body)
  ;; FIXME: define-fwrapper docstring
  (multiple-value-bind (body decls doc intr)
      (parse-body body)
    `(%store-fwrapper
      (make-fwrapper
       :name ',name :documentation ,doc
       :thunk ,(gen-fwrapper-thunk name fw-lambda-list decls body)))))
(set-indent 'define-fwrapper 'defun)

;;;;; apply/execute fwrapper
;;
;; NOTE: fwrappers are applied to a function first. then it is executed
;; later when the fwrapped function is called.

(defparameter *this-function* nil)

(lisp:defun call-next-function (&rest next-args)
  (error "call-next-function called out of fwrapper."))

(lisp:defun %ensure-primary-function-backup (fn)
  "Store things needed to rebuild the primary function."
  (unless (function-info fn 'primary-function)
    ;; Assume FN is not fwrapped yet.
    (setf (function-info fn 'primary-function)
          (let ((whole (si:closure-body fn))
                ;(name  (function-name fn))
                )
            (list (second whole) ; lambda-list
                  (cddr whole)   ; body forms
                  ;(when (symbolp name) (documentation name 'function))
                  )))))

(lisp:defun %fwrapper-thunk-list (active-fwrappers) ; being a plist (INDICATOR FWRAPPER...)
  (let ((thunks '()))
    (do ((rest active-fwrappers (cddr rest)))
        ((null rest))
      (push (fwrapper-thunk (second rest)) thunks))
    (nreverse thunks)))

(lisp:defun %finalize-fwrapped-function (fn)
  (let ((primary    (function-info fn 'primary-function))
        (fwrappers  (function-info fn 'active-fwrappers))
        (closure    (si:closure-body fn)))
    (if (null fwrappers)
      ;; restore primary function
      (setf (second closure) (first primary)
            (cddr closure)   (second primary))
      ;; wrap the function
      (let ((next-function-list
              (append (%fwrapper-thunk-list fwrappers)
                      (list `(lambda (#:next-fns ,@(copy-list (first primary)))
                               ,@(second primary))))))
        (setf (second closure) `(&rest initial-args
                                 &aux (,+fn-info-marker+ ,(%fn-info-table fn)))
              (cddr closure)   `((let ((*this-function* ,fn))
                                   (apply ,(first next-function-list)
                                          (list ,@(rest next-function-list))
                                          initial-args))))))))


(lisp:defun fwrap (function fwrapper &optional (indicator (gensym "fwrap-")))
  ;; FIXME: check the args
  (%ensure-primary-function-backup function)
  (setf (getf (function-info function 'active-fwrappers) indicator) (find-fwrapper fwrapper))
  (%finalize-fwrapped-function function)
  function)

(lisp:defun funwrap (function &optional indicator)
  (if indicator
    (remf (function-info function 'active-fwrappers) indicator)
    (setf (function-info function 'active-fwrappers) nil))
  (%finalize-fwrapped-function function)
  function)

;;;; trace

(defparameter *trace-depth* nil)

(define-fwrapper trace (&rest args)
  (let ((*trace-depth* (1+ (or *trace-depth* -1))))
    (format t "~&~V,0Ttrace> (~S ~{~S~^ ~})~%"
            *trace-depth*
            *this-function*
            ;(function-name *this-function*)
            args)
    (let ((values (multiple-value-list (call-next-function))))
      (format t "~:{~&~V,0T=> ~S~}"
              (mapcar (lambda (val)
                        (list *trace-depth* val))
                      values))
      (values-list values))))

(defparameter *traced-functions* nil)

;; [CLHS: Macro TRACE, UNTRACE]: http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm
(defmacro trace (&rest function-names)
  `(if ',function-names
     (mapc (lambda (name)
             (unless (or (symbolp name)
                         (setf-function-name-p name))
               (error 'type-error
                      :datum name
                      :expected-type 'function-name)) ;FIXME: undefined typespec
             (pushnew name *traced-functions*)
             (fwrap (fdefinition name) 'trace 'trace))
           ',function-names)
     *traced-functions*))

(defmacro untrace (&rest function-names)
  `(mapc (lambda (name)
           (unless (or (symbolp name)
                       (setf-function-name-p name))
             (error 'type-error
                    :datum name
                    :expected-type 'function-name)) ;FIXME: undefined typespec
           (funwrap (fdefinition name) 'trace)
           (setf *traced-functions* (delete name *traced-functions*)))
         (or ',function-names
             *traced-functions*)))



(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro defun (name (&rest lambda-list) &body body)
    (labels ((warn-redefinition (sym name)
               `(when (and (fboundp ',sym)
                           *warn-on-redefine-function*)
                  (warn 'redefining-function :name ',name :type 'function))))
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
                    (list ',sym ,@lambda-list)))))))))
(set-indent 'defun 'defun)

;; http://www.lispworks.com/documentation/HyperSpec/Body/f_fdefin.htm

(lisp:defun fdefinition (function-name)
  (symbol-function (canonicalize-function-name function-name)))

(defsetf fdefinition (function-name) (new-definition)
  (let (($name (gensym "$name-"))
        ($reader (gensym "$reader-")))
    `(progn
       (unless (functionp ,new-definition)
         (error 'type-error :datum ,new-definition :expected-type 'function))
       (cond ((setf-function-name-p ,function-name)
              (let ((,$name (canonicalize-function-name ,function-name))
                    (,$reader (second ,function-name)))
                (progn
                  ;; Store setf-expander for READER
                  ;; This is mostly an expansion of `defsetf`
                  (si:*putprop ,$reader
                               `((&rest #1=#:args) (#2=#:new-value)
                                 `(,',,$name ,#2# ,@#1#))
                               'lisp::setf-lambda)
                  (remprop ,$reader 'lisp::setf-method)
                  (remprop ,$reader 'lisp::setf-update))
                (setf (symbol-function ,$name) ,new-definition)))
             (t
              (setf (symbol-function ,function-name) ,new-definition))))))

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
