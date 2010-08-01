;;; -*- mode: lisp; package: cl.pprint -*-
;;;
;;; common-lisp/pprint.l
;;;

;;;;
;;;; * Package

(require "common-lisp/package")

(in-package :common-lisp)

(export '(pprint
          define-pprint))


(defpackage :cl.pprint
  (:use :lisp :editor :common-lisp)
  (:nicknames "common-lisp.pprint"))

(in-package :cl.pprint)

;;;;
;;;; * Utilities

;; from On Lisp
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defmacro output-to-string-with-indent-as-sexp (&body body)
  (let (($buffer (gensym)))
    `(with-output-to-temp-buffer (" *OUTPUT-TO-STRING-WITH-INDENT-AS-SEXP*" nil)
       (let ((,$buffer (buffer-stream-buffer *standard-output*)))
         (with-set-buffer
           (set-buffer ,$buffer)
           (use-syntax-table ed::*lisp-mode-syntax-table*)
           (macrolet ((setq-local (var val &rest more)
                        `(progn
                           (make-local-variable ',var)
                           (setq ,var ,val)
                           ,(when more `(setq-local ,@more)))))
             (setq-local mode-specific-indent-command 'lisp-indent-line
                         indent-tabs-mode nil))
           ,@body
           (indent-region (point-min) (point-max))
           ;; return everything get printed
           (buffer-substring (point-min) (point-max)))))))


(defmacro with-ca/dr ((cons &optional (car-name 'car) (cdr-name 'cdr)) &body body)
  (let (($cons (gensym)))
    `(let* ((,$cons ,cons)
            (,car-name (car ,$cons))
            (,cdr-name (cdr ,$cons)))
       ,@body)))


(defmacro LFD () (princ #\LFD))
(defmacro SPC () (princ #\SPC))
(defmacro WRAP (&body body)
  `(progn (princ #\() ,@body (princ #\))))


;;; for (operator . args) given as FORM
;;; - first N args get printed to same line with OPERATOR with separating
;;;   them by space.
;;; - args after N grouped by STEP and each group get printed to their
;;;   own line.
;;;
;;; * (pprint-form-1 '(defun name (arg0 arg1) foo bar) 2)
;;; (defun name (arg0 arg1)
;;; foo
;;; bar)
;;; => ...
;;;
;;; * (pprint-form-1 '(setq x 10 y 20 z 30) 2 2)
;;; (setq x 10
;;; y 20
;;; z 30)
;;; => ...
(defun pprint-form-1 (form &optional (n 1) (step 1))
  (let* ((operator (car form))
         (args (cdr form))
         (beginning (subseq args 0 (min n (length args))))
         (rest (group (nthcdr n args) step)))
    (WRAP
     (princ operator)
     (dolist (x beginning)
       (SPC)
       (pprint-sexp x))
     (dolist (set rest)
       (LFD)
       (pprint-sexp (car set))
       (dolist (x (cdr set))
         (SPC)
         (pprint-sexp x))))
    (values operator beginning rest))) ; returning them for debugging porpose

(setf (get 'pprint-form-1 'ed::lisp-indent-hook) 1)

(defun pprint-args (args)
  (princ args))

(defun pprint-body (body)
  (dolist (x body)
    (LFD)
    (pprint-sexp x)))

(defun pprint-forms (forms)
  (pprint-sexp (car forms))
  (dolist (x (cdr forms))
    (SPC)
    (pprint-sexp x)))


;;;
;;; for specific operators

(defparameter *pprint-table* (make-hash-table))

(defmacro define-pprint (operator args &body rest)
  `(setf
    (gethash ',operator *pprint-table*)
    ,(cond ((consp args) `(lambda ,args ,@rest))
           ((symbolp args) `(gethash ',args *pprint-table*))
           ((numberp args) `(lambda (&rest $form)
                              (pprint-form-1 $form ,args ,@rest))))))

(define-pprint setf 2 2)
(define-pprint setq setf)
(define-pprint psetf setf)
(define-pprint psetq setf)

(define-pprint if 1)

(define-pprint lambda 1)

(define-pprint defun 2)
(define-pprint defmacro defun)
(define-pprint defsetf defun)

(define-pprint and 1)
(define-pprint or and)


;;; - flet

(defun %pprint-flet-def (def)
  (if (symbolp def)
      (princ def)
    (WRAP
     (princ (car def)) (SPC)
     (pprint-args (cadr def))
     (pprint-body (cddr def)))))

(define-pprint flet (operator defs &rest body)
  (WRAP
   (princ operator) (SPC)
   (WRAP
    (%pprint-flet-def (car defs))
    (dolist (def (cdr defs))
      (LFD) (%pprint-flet-def def)))
   (pprint-body body)))

(define-pprint let flet)
(define-pprint let* flet)
(define-pprint labels flet)
(define-pprint macrolet flet)
(define-pprint prog flet)

;;; - do
(define-pprint do (operator bindings exit &rest body)
  (WRAP
   (princ operator) (SPC)
   (WRAP
    (WRAP (pprint-forms (car bindings)))
    (dolist (x (cdr bindings))
      (LFD)
      (WRAP (pprint-forms x))))
   (LFD)
   (WRAP
    (pprint-sexp (first exit))
    (when (second exit)
      (LFD) (pprint-sexp (second exit))))
   (pprint-body body)))

(define-pprint do* do)

;;; - case, cond

(defun %pprint-clause (clause)
  (WRAP
   (if (consp (car clause))
       (WRAP (pprint-forms (car clause)))
     (pprint-sexp (car clause)))
   (pprint-body (cdr clause))))

(define-pprint case (operator key &rest clauses)
  (WRAP
    (princ operator) (SPC)
    (pprint-sexp key)
    (dolist (c clauses)
      (LFD) (%pprint-clause c))))

(define-pprint ecase case)
(define-pprint ccase ccase)
(define-pprint typecase case)
(define-pprint etypecase case)
(define-pprint ctypecase case)

(define-pprint cond (operator &rest clauses)
  (WRAP
   (princ operator) (SPC)
   (dolist (c clause)
     (LFD) (%pprint-clause c))))

;; - handler-case
(define-pprint handler-case (operator form &rest handlers)
  (WRAP
   (princ operator) (LFD)
   (pprint-sexp form)
   (dolist (h handlers)
     (LFD)
     (WRAP
      (if (symbolp (car h))
          (princ (car h))
        (WRAP (pprint-forms (car h))))
      (SPC)
      (pprint-args (cadr h))
      (pprint-body (cddr h))))))

;; - handler-bind
(define-pprint handler-bind (operator handlers &rest body)
  (labels ((print-handler (handler)
             (let ((conditions (car handler))
                   (handler (cadr handler)))
               (WRAP
                (if (symbolp conditions)
                    (princ conditions)
                  (WRAP (pprint-forms conditions)))
                (LFD)
                (pprint-sexp handler)))))
    (WRAP
     (princ operator) (SPC)
     (WRAP
      (print-handler (car handlers))
      (dolist (h (cdr handlers))
        (LFD)
        (print-handler h)))
     (pprint-body body))))

;; - loop
;; - defstruct
(define-pprint defstruct (operator name-and-options &rest slots)
  (WRAP
   (princ operator) ;defstruct
   (SPC)
   (if (symbolp name-and-options)
       (princ name) ;NAME
     (with-ca/dr (name-and-options name options)
       (WRAP
        (princ name) ;NAME
        (dolist (o options)
          (LFD)
          (if (symbolp o)
            (prin1 o)
            (WRAP
             (prin1 (car o))
             (if (and (eql (car o) :print-function)
                      (consp (cadr o))
                      (eql (car (cadr o)) 'lambda))
               (LFD)
               (SPC))
             (pprint-forms (cdr o)) ))))))
   (dolist (s slots)
     (LFD)
     (if (symbolp s)
         (princ s)
       (WRAP
        (princ (car s)) (SPC)
        (pprint-forms (cdr s)))))))

;; - defpackage
(define-pprint defpackage (operator name &rest options)
  (WRAP
   (princ operator)
   (SPC)
   (prin1 name)
   (dolist (o options)
     (LFD)
     (WRAP (pprint-forms o)))))


;;;
;;; for macro call

(defun macro-with-body-p (symbol)
  (check-type symbol symbol)
  (and (macro-function symbol)
       (member '&body (cadr (macro-function symbol)))))

(defun %macro-n-args-before-&body (macro-symbol)
  (let ((params (cadr (macro-function macro-symbol))))
    (do ((x (car params) (car rest))
         (rest (cdr params) (cdr rest))
         (n 0))
        ((member x '(nil &body)) n)
      (if (member x '(&whole &environment))
          (setf rest (cdr rest))
        (incf n)))))

(defun pprint-macro-call (form)
  (pprint-form-1 form
    (%macro-n-args-before-&body (car form))))

;;;
;;; for ordinary function call

(defun pprint-function-call (form)
  (pprint-form-1 form
    (let ((n-args (length (cdr form))))
      (if (> n-args 3) 1 n-args))))


(defun pprint-form (form)
  (with-ca/dr (form operator args)
    (assert (symbolp operator))
    (cond
     ((gethash operator *pprint-table*)
      (apply (gethash operator *pprint-table*) form))
     ((macro-with-body-p operator)
      (pprint-macro-call form))
     ((numberp (get operator 'ed:lisp-indent-hook))
      (pprint-form-1 form (get operator 'ed:lisp-indent-hook)))
     (t
      (pprint-function-call form)))))


(defun pprint-sexp (object)
  (if (and (consp object)
           (symbolp (car object)))
      (pprint-form object)
    (prin1 object)))


(defun cl:pprint (object &optional (stream *standard-output*))
  (LFD)
  (princ (output-to-string-with-indent-as-sexp
           (pprint-sexp object))
         stream)
  (values))


(provide "common-lisp/pprint")

;;; common-lisp/pprint.l ends here.
