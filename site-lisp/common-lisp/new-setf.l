;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/new-setf.l
;;;
;;; - Support (setf ...) function
;;; - Support (setf (values ..) ..)
;;; - Renaming
;;;   - define-setf-method -> define-setf-expander
;;;   - get-setf-method-multiple-value -> get-setf-expansion
;;;
#|
    (setf (ACCESSOR . ARGS) NEW-VALUE)

Properties where SETF-EXPANDERs stored
======================================
xyzzy stores setf expander in property of ACCESSOR symbol.

Property lisp::setf-lambda
--------------------------
A list (VARS NEW-VALUE . BODY) which is everything given on long form of
DEFSETF.  These forms are used to construct a lambda expression which get
called with ARG-VARS and NEW-VALUE-VAR and produces setf expansion in
GET-SETF-METHOD-MULTIPLE-VALUE.

Property lisp::setf-update
--------------------------
A function symbol given on short form of DEFSETF.  When this function
found, setf form would get expanded into something like following:

    (UPDATE-FN ARG-0-VAR ARG-1-VAR .. ARG-N-VAR NEW-VALUE-VAR)

Property lisp::setf-method
--------------------------
A function defined by DEFINE-SETF-METHOD.  Simply invoked with ENV and
ARGS in GET-SETF-METHOD-MULTIPLE-VALUE.
Since Common Lisp rename 'setf method' to 'setf expander' and Macro
DEFINE-SETF-METHOD to DEFINE-SETF-EXPANDER, using new name for property
as well would be better option, but to keep non-cl version alive, I have
to rely on old name.

|#

(require "common-lisp/package")

(in-package :common-lisp)

(export '(define-setf-expander
          get-setf-expansion
          fdefinition))

(shadowing-export '(setf
                    fboundp
                    fmakunbound))

;;;;
;;;; * Utilities

(defmacro %acond (&body clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym)) ,@(cdr cl1))
           (%acond ,@(cdr clauses)))))))

(defun %n-gensyms (n)
  "Return list of gensym."
  (let ((ret nil))
    (dotimes (i n ret) (push (gensym) ret))))


(defun %setf-function-name-p (name)
  "Return true if NAME denotes setf function."
  (and (consp name)
       (member (car name) '(lisp:setf cl::setf))
       (and (cadr name)
            (symbolp (cadr name)))
       (null (cddr name))))

(defun %function-symbol (name)
  "Return symbol which holds function it its function cell."
  (cond ((symbolp name) name)
        ((%setf-function-name-p name)
         (intern (format nil "(setf ~A)" (cadr name))
                 (symbol-package (cadr name))))
        (t (error 'type-error :datum name
                  :expected-type '(or symbol cons)))))


;; - Rename: define-setf-method -> define-setf-expander
(defmacro define-setf-expander (access-fn lambda-list &body body)
  `(define-setf-method ,access-fn ,lambda-list ,@body))

;;;;
;;;; * Expanding setf form

;; - Rename: get-setf-method-multiple-value -> get-setf-expansion
;; almost Copy'n'Pasted from lisp/setf.l
(defun get-setf-expansion (form &optional env)
  (%acond ((symbolp form)
           (let ((store (gensym)))
             (values nil nil (list store) (list 'setq form store) form)))
          ((or (not (consp form))
               (not (symbolp (car form))))
           (error "~S の setf expander はありません。" form))
          ((si:*find-in-environment form env)
           (multiple-value-bind (expansion expanded-p)
               (macroexpand-1 form env)
             (unless expanded-p
               (error "~S の setf expander はありません。" form))
             (get-setf-expansion expansion env)))
          ((get (car form) 'lisp::setf-method)
           (apply it env (cdr form)))
          ((get (car form) 'lisp::setf-update)
           (let ((vars (%n-gensyms (length (cdr form))))
                 (store (gensym)))
             (values vars (cdr form) (list store)
                     `(,it ,@vars ,store) (cons (car form) vars))))
          ((get (car form) 'lisp::setf-lambda)
           (let* ((vars (%n-gensyms (length (cdr form))))
                  (store (gensym)))
             (values vars (cdr form) (list store)
                     (apply `(lambda (,@(cadr it) ,@(car it)) ,@(cddr it))
                            (cons store vars))
                     (cons (car form) vars))))
          ((and (not (special-form-p (car form)))
                (macro-function (car form)))
           (multiple-value-bind (expansion expanded-p)
               (macroexpand-1 form env)
             (unless expanded-p
               (error "~S の setf expander はありません。" form))
             (get-setf-expansion expansion env)))
          (t
           (error "setf フォームを展開できません: ~S" form))
          ))


(defun expand-setf-1 (place newvalue env)
  (if (atom place)
    `(setq ,place ,newvalue)
    (let (f)
      (when (and (null (get (car place) 'lisp::setf-update))
                 (null (get (car place) 'lisp::setf-lambda)))
        (multiple-value-setq (place f) (macroexpand-1 place env))
        (when f
          (return-from expand-setf-1
            (expand-setf-1 place newvalue env))))
      (when (setq f (get (car place) 'lisp::setf-update))
        (return-from expand-setf-1
          `(,f ,@(cdr place) ,newvalue)))
      (multiple-value-bind (vars vals stores store-form access-form)
          (get-setf-expansion place env)
        ;(multiple-value-setq
        ;    (vars vals stores store-form access-form newvalue)
        ;    ;; should be named OPTIMIZE-SETF-EXPANSION
        ;    (optimize-setf-method vars vals stores store-form
        ;                          access-form (list newvalue)))
        (let ((vars (append vars stores))
              (vals (append vals (if (= (length stores) 1)
                                   (list newvalue)
                                   (mapcar (lambda (#:ignore) nil) stores)))))
          (cond ((null vars) store-form)
                ((= (length stores) 1)
                 `(let* (,@(mapcar #'list vars vals))
                    ,store-form))
                (t ;setf'ing multiple values
                 `(let* (,@(mapcar #'list vars vals))
                    (multiple-value-setq (,@stores) ,newvalue)
                    ,store-form))))))))

;; just copied from lisp/setf.l
(defun expand-setf (args env)
  (do ((al args (cddr al))
       (result '()))
      ((endp al) (nreverse result))
    (if (endp (cdr al))
      (error "不正な setf フォームです: (setf ~S)" (car al)))
    (setq result (cons (expand-setf-1 (car al) (cadr al) env) result))))

(defmacro setf (&rest args &environment env)
  (cond ((endp args)
         nil)
        ((endp (cdr args))
         (error "不正な setf フォームです: (setf~{ ~S~})" args))
        ((endp (cddr args))
         (expand-setf-1 (car args) (cadr args) env))
        (t (cons 'progn (expand-setf args env)))))


;;;
;;; * Setf Expander for VALUES
(define-setf-expander values (&rest places)
  (let ((stores (%n-gensyms (length places))))
    (values nil nil stores
            `(values ,@(mapcar (lambda (place store)
                                 `(setf ,place ,store))
                         places
                         stores))
            `(values ,@stores))))

;;;
;;; * fdefinition
;;;
;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fdefin.htm
;;;

(defun fdefinition (name)
  (symbol-function (%function-symbol name)))

(defun %set-fdefinition (name new-definition)
  (setf (symbol-function (%function-symbol name)) new-definition)
  new-definition)

(defsetf fdefinition %set-fdefinition)


;;;
;;; * fboundp and fmakunbound
;;;
;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fbound.htm
;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/f_fmakun.htm
;;;

(defun fboundp (name)
  (lisp:fboundp (%function-symbol name)))

(defun fmakunbound (name)
  (lisp:fmakunbound (%function-symbol name))
  name)



(provide "common-lisp/new-setf")

#|
NOTE:
- This file is intended to be a replacement of site-lisp/common-lisp/setf-function-name.l
  and already includes several duplicated definitions.
|#

;;; common-lisp/setf.l ends here.
