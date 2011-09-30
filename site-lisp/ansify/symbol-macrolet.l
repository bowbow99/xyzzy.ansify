;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/symbol-macrolet.l --- pseudo `symbol-macrolet` implemented as macro
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (see COPYING)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/setf"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (shadowing-export '(#:symbol-macrolet
                      )))

;;;;
;;;; * utilities

(defun sm.merge-env (env type local-defs)
  "Return an environment object which contains everything in ENV plus
 local function/macros from LOCAL-DEFS."
  (evalhook (let ((env (gensym)))
              `(,type ,local-defs
                (macrolet ((,env (&environment ,env) ,env)) (,env))))
            nil nil env))

(defun sm.shadow-symbol (sym/expansions shadowed)
  (remove-if (lambda (sym/expansion)
               (find (car sym/expansion) shadowed))
    sym/expansions))

;;;;
;;;; * Expand Forms

(defun sm.expand-form (form sym/expansions &optional env)
  (setq form (macroexpand form env))
  (labels ((rec (form)
             (sm.expand-form form sym/expansions env))
           (pass (fn)
             (funcall fn form sym/expansions env)))
    (if (atom form)
      (sm.expand-atom form sym/expansions env)
      (case (car form)
        (quote    form)
        (setq     `(setf ,@(mapcar #'rec (cdr form))))
        (let      (pass 'sm.expand-let))
        (let*     (pass 'sm.expand-let*))
        (macrolet (pass 'sm.expand-macrolet))
        (flet     (pass 'sm.expand-flet))
        (labels   (pass 'sm.expand-labels))
        (lambda   (pass 'sm.expand-lambda))
        (tagbody  (pass 'sm.expand-tagbody))
        (multiple-value-setq  (pass 'sm.expand-mv-setq))
        (multiple-value-bind  (pass 'sm.expand-mv-bind))
        (function
         (if (consp (cadr form))
           `(function ,(sm.expand-lambda (cadr form) sym/expansions env))
           form))
        ((throw catch return-from go block)
         `(,(car form) ,(cadr form) ,@(mapcar #'rec (cddr form))))
        (t
         `(,(if (and (consp (car form))
                     (eql (caar form) 'lambda))
              (sm.expand-lambda (car form) sym/expansions env)
              (car form))
           ,@(mapcar #'rec (cdr form))))))))


(defun sm.expand-atom (form sym/expansions &optional env)
  (let ((found (find form sym/expansions :key #'car)))
    (if found
      (cadr found)
      form)))

(defun sm.expand-body (body sym/expansions &optional env)
  (mapcar (lambda (form)
            (sm.expand-form form sym/expansions env))
    body))

(defun sm.expand-tagbody (form sym/expansions &optional env)
  `(tagbody
     ,@(mapcar (lambda (form)
                 (if (symbolp form) form
                   (sm.expand-form form sym/expansions env)))
         (cdr form))))

(defun sm.expand-macrolet (form sym/expansions &optional env)
  (labels ((sm.expand-def (def)
             (multiple-value-bind (lambda-list% sym/expansions%)
                 (sm.expand-lambda-list (cadr def) sym/expansions env)
               `(,(car def) ; NAME
                 ,lambda-list%
                 ,@(mapcar (lambda (form)
                             (sm.expand-form form sym/expansions% env))
                     (cddr def))))))
    (let* ((defs (mapcar #'sm.expand-def (cadr form)))
           (env (sm.merge-env env 'macrolet defs)))
      `(macrolet (,@defs)
         ,@(sm.expand-body (cddr form) sym/expansions env)))))

(defun sm.expand-mv-setq (form sym/expansions &optional env)
  (unless (= (length form) 3)
    (error "不正な multiple-value-setq フォームです: ~S" form))
  (let ((places (mapcar (lambda (x)
                          (sm.expand-atom x sym/expansions env))
                  (second form)))
        (new-values-form (third form))
        (expansions
          ;; NOTE: In short, holds (temp-vars val-forms store-vars store-forms access-forms).
          ;; See [CLHS: Section 5.1.1.2 Setf Expansions] for more details.
          ;; - http://www.lispworks.com/documentation/HyperSpec/Body/05_aab.htm
          (list nil nil nil nil nil)))
    (dolist (place places)
      (setf expansions
            (mapcar #'cons
              (multiple-value-list (get-setf-expansion place env))
              expansions)))
    (setf expansions (mapcar #'nreverse expansions))
    (let ((temp-vars (apply #'append (first expansions)))
          (val-forms (apply #'append (second expansions))))
      `(multiple-value-bind (,@(mapcar #'car (third expansions)))
           ,new-values-form
         (let* (,@(mapcar #'list temp-vars val-forms))
           ,@(fourth expansions)
           ,(caar (third expansions)))))))

(defun sm.expand-mv-bind (form sym/expansions &optional env)
  (let ((syms (cadr form))
        (values (caddr form))
        (body (cdddr form)))
    `(multiple-value-bind ,syms ,(sm.expand-form values sym/expansions env)
       ,@(sm.expand-body body (sm.shadow-symbol sym/expansions syms) env))))

(defun sm.expand-let (form sym/expansions &optional env)
  (let ((letted (mapcar (lambda (letting)
                          (if (symbolp letting) letting
                            (car letting)))
                  (cadr form))))
    `(let ,(mapcar (lambda (letting)
                     (cond ((consp letting)
                            (list (car letting)
                                  (sm.expand-form (cadr letting) sym/expansions env)))
                           ((symbolp letting) letting)
                           (t (error "不正な let フォームです:~%~S" form))))
             (cadr form))
       ,@(sm.expand-body (cddr form) (sm.shadow-symbol sym/expansions letted) env))))

(defun sm.expand-let* (form sym/expansions &optional env)
  (let ((letted nil))
    (labels ((sm.expand-with-shadow (form)
               (prog1
                   (sm.expand-form (cadr form) (sm.shadow-symbol sym/expansions letted) env)
                 (push (car form) letted))))
      `(let* ,(mapcar (lambda (pair)
                        (cond ((consp pair)
                               (list (car pair)
                                     (sm.expand-with-shadow pair)))
                              ((symbolp pair) pair)
                              (t (error "不正な let* フォームです: ~S" form))))
                (cadr form))
         ,@(sm.expand-body (cddr form) (sm.shadow-symbol sym/expansions letted) env)))))


(defun sm.expand-lambda-list (lambda-list sym/expansions &optional env)
  (macrolet ((shadow-sym/expansions (sym)
               `(setf sym/expansions (sm.shadow-symbol sym/expansions (list ,sym)))))
    (labels ((*expand (*lambda-list &optional (state '&required))
               (cond ((null *lambda-list) nil)
                     ((symbolp *lambda-list)
                      (shadow-sym/expansions *lambda-list)
                      *lambda-list)
                     ((member (first *lambda-list) lambda-list-keywords :test #'eq)
                      (let ((new-state (first *lambda-list)))
                        (case new-state
                          ((&whole &environment)
                           (shadow-sym/expansions (second *lambda-list))
                           `(,new-state
                             ,(second *lambda-list)
                             ,@(*expand (cddr *lambda-list) state)))
                          (&rest
                           `(,new-state
                             ,(*expand (second *lambda-list)) ; possibly destructuring
                             ,@(*expand (cddr *lambda-list))))
                          (t
                           `(,new-state ,@(*expand (cdr *lambda-list) new-state))))))
                     (t
                      (let ((param (first *lambda-list)))
                        (case state
                          (&required
                           (shadow-sym/expansions param)
                           `(,(*expand param) ; possibly destructuring
                             ,@(*expand (cdr *lambda-list) state)))
                          ((&optional &key &aux)
                           `(,(cond ((symbolp param)
                                     (shadow-sym/expansions param)
                                     param)
                                    ((consp param)
                                     (let* ((len (length param))
                                            (init-form (if (>= len 2)
                                                         (sm.expand-form (second param) sym/expansions env))))
                                       (shadow-sym/expansions (if (consp (first param))
                                                                ;; ((keysym var) ..) in &key parameters
                                                                (second (first param))
                                                                (first param)))
                                       (when (= len 3)
                                         (shadow-sym/expansions (third param)))
                                       `(,(first param)
                                         ,@(when (>= len 2) (list init-form))
                                         ,@(when (>= len 3) (list (third param)))))))
                             ,@(*expand (cdr *lambda-list) state)))))))))
      (values (*expand lambda-list)
              sym/expansions))))

(defun sm.expand-lambda (form sym/expansions &optional env)
  (multiple-value-bind (lambda-list% sym/expansions%)
      (sm.expand-lambda-list (cadr form) sym/expansions env)
    `(lambda ,lambda-list%
       ,@(sm.expand-body (cddr form) sym/expansions% env))))

(defun sm.expand-flet (form sym/expansions &optional env)
  `(flet
       ;; DEFINITIONS of local functions
       ,(mapcar (lambda (def)
                  (multiple-value-bind (lambda-list% sym/expansions%)
                      (sm.expand-lambda-list (cadr def) sym/expansions env)
                    `(,(car def) ; NAME
                      ,lambda-list%
                      ,@(mapcar (lambda (form)
                                  (sm.expand-form form sym/expansions% env))
                          (cddr def)))))
          (cadr form))
     ,@(sm.expand-body (cddr form) sym/expansions (sm.merge-env env 'flet (cadr form)))))

(defun sm.expand-labels (form sym/expansions &optional env)
  (let ((new-env env))
    `(labels
         ;; DEFINITIONS of local function
         ,(mapcar (lambda (def)
                    (setf new-env (sm.merge-env new-env 'labels (list def)))
                    (multiple-value-bind (lambda-list% sym/expansions%)
                        (sm.expand-lambda-list (cadr def) sym/expansions env)
                      `(,(car def) ; NAME
                        ,lambda-list%
                        ,@(mapcar (lambda (form)
                                    (sm.expand-form form sym/expansions% new-env))
                            (cddr def)))))
            (cadr form))
       ,@(sm.expand-body (cddr form) sym/expansions new-env))))


(defmacro symbol-macrolet (sym/expansions &body forms &environment env)
  (multiple-value-bind (decl forms)
      (lisp::find-declaration forms)
    (let ((em (and decl (eql (caadr decl) 'special)
                   (intersection (cdadr decl) (mapcar #'car sym/expansions)))))
      (when em (error "declaring symbol macros as special:~{ ~S~}" em)))
    (let ((constants (remove-if-not #'constantp
                       (mapcar #'car sym/expansions))))
      (when constants
        (error "constant variables cannot be a symbol macro:~{ ~S~}" constants)))
    `(progn
      ,@(mapcar (lambda (form)
                  (sm.expand-form form sym/expansions env))
          forms))))
#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

(provide "ansify/symbol-macrolet")
;;; ansify/symbol-macrolet.l ends here
