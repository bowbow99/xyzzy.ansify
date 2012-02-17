;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/restart.l
;;;
;;; Author:     bowbow99 <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:


;;;; Package

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify)

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; CL standard symbols
  ;; NOTE: SHADOWING-EXPORT'ing them because they conflict with symbols from
  ;; old "condition-restart".
  (shadowing-export '(;; Restart establishing macros
                      #:restart-case
                      #:restart-bind
                      #:with-simple-restart
                      #:with-condition-restarts
                      ;; Restart manipulating functions
                      #:restart-name
                      #:compute-restarts
                      #:find-restart
                      #:invoke-restart
                      #:invoke-restart-interactively
                      ;; Condition Signaller
                      #:cerror
                      #:warn
                      ;; Standard restart names
                      #:abort
                      #:continue
                      #:muffle-warning
                      #:use-value
                      #:store-value
                      ;; as a typespec
                      #:restart
                      ))

  (export-from :ansify.ext
               '(#:read-value
                 ))

  ) ; end of `eval-when`


;;;; Restart Object

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun print-restart (restart stream #:what?)
    (labels ((name ()
               (or (restart-name restart)
                   "(anonymous)")))
      (cond (*print-readably* (print-structure-readably restart stream))
            (*print-escape*   (format stream "#<restart: ~A>" (name)))
            (t (aif (restart-report-function restart)
                 (funcall it stream)
                 (princ (name) stream)))))))

(defstruct (restart
            (:print-function print-restart))
  name
  function
  interactive-function
  report-function
  (test-function (lambda (&optional condition) t)))

(defparameter *active-restarts* nil
  "List of active restart.")

(defparameter *condition/restart-association* nil
  "List of condition and associated restarts.")


;;;; Macro restart-bind

(defmacro restart-bind ((&rest restarts) &body body)
  "Evaluate the BODY in a dynamic environment where RESTARTS are in effect."
  `(let ((*active-restarts*
           (append (list ,@(mapcar (lambda (restart)
                                     `(make-restart :name ',(car restart)
                                                    :function ,(cadr restart)
                                                    ,@(cddr restart)))
                             restarts))
                   *active-restarts*)))
     ,@body))
#+xyzzy
(setf (get 'restart-bind 'ed:lisp-indent-hook) 1
      (get 'restart-bind 'ed::lisp-indent-flet) t)


;;;; Macro with-condition-restarts

(defmacro with-condition-restarts (condition restarts &body body)
  "Evaluate BODY with associating RESTARTS with CONDITION."
  `(let ((*condition/restart-association*
           (let ((#1=#:key ,condition)
                 (#2=#:datum ,restarts))
             (cons (cons #1# (union #2# (cdr (assoc #1# *condition/restart-association*))))
                   (remove #1# *condition/restart-association*
                           :key #'car)))))
     ,@body))
#+xyzzy (set-indent 'with-condition-restarts 2)


;;;; Macro restart-case

(eval-when (:load-toplevel :compile-toplevel :execute)

  (defun rs.transform-keyword-option-report (form)
    (list :report-function
          (case (type-of form)
            (simple-string
             `(lambda (#1=#:stream) (format #1# ,form)))
            (symbol
             (if form
               `',form
               (error "Invalid function for :report option: ~S" form)))
            (cons ; lambda form
             (if (eql 'lambda (car form))
               form
               (error "Invalid function for :report option: ~S" form)))
            (t (error "Invalid function for :report option: ~S" form)))))

  (defun rs.transform-keyword-option-test (form)
    (list :test-function `#',form))

  (defun rs.transform-keyword-option-interactive (form)
    (list :interactive-function `#',form))

  (defun rs.transform-keyword-option (keyword/form)
    (let ((keyword (car keyword/form))
          (form (cadr keyword/form)))
      (funcall (case keyword
                 (:report 'rs.transform-keyword-option-report)
                 (:interactive 'rs.transform-keyword-option-interactive)
                 (:test 'rs.transform-keyword-option-test)
                 (t (error "Invalid keyword option for restart-case clause: ~S"
                           keyword)))
               form)))
  )

(defmacro rs.with-tag/clause ((tag/clause) &body body)
  `(let ((tag (first ,tag/clause))
         (name (second ,tag/clause))
         (args (third ,tag/clause))
         (options nil)
         (body (cdddr ,tag/clause)))
     (while (and (keywordp (car body))
                 (member (car body) '(:report :interactive :test)))
       (setf options (cons (subseq body 0 2) options)
             body (cddr body)))
     (setf options (nreverse options))
     ,@body))
#+xyzzy (set-indent 'rs.with-tag/clause 'defun)

(defun rs.make-condition (datum args default)
  (case (type-of datum)
    ((simple-string string)
     (make-condition default :format-string datum :format-arguments args))
    (symbol
     (apply #'make-condition datum args))
    (t
     (if (and (si:*structurep datum)
              (si:*structure-subtypep (si:*structure-definition datum)
                                      (get 'condition 'si:structure-definition))
              (null args))
       datum
       (error 'type-error
              :datum datum
              :expected-type '(or condition symbol string))))))

(defun rs.expand-signalling-form (form restart-names)
  "Expands auto-associating form out of condition signalling form FORM."
  (labels ((parse-signalling-form (form)
             "Return    operator     report        datum         args"
             (case (car form)
               (cerror
                (values (first form) (second form) (third form)  (cdddr form)))
               ((error warn signal)
                (values (first form) nil           (second form) (cddr form)))
               (t
                (error "Invalid signalling form: ~S" form))))
           (default-type (operator)
             (case operator
               ((cerror error) 'simple-error)
               (warn 'simple-warning)
               (signal 'simple-condition))))
    (multiple-value-bind (operator report datum args default-type)
        (parse-signalling-form form)
      `(let ((#1=#:condition (rs.make-condition ,datum (list ,@args)
                                              ',(default-type operator))))
         (with-condition-restarts #1# (mapcar #'find-restart '(,@restart-names))
           ,(if (eql operator 'cerror)
              `(cerror ,report #1# ,@args)
              `(,operator #1#)))))))

(defmacro restart-case (form &rest clauses)
  "Evaluate the FORM in a environment where clauses establishes restarts."
  (let ((tags/clauses (mapcar (lambda (c) (cons (gensym) c)) clauses)))
    `(block #1=#:restart-case-root
       (let ((#2=#:restart-args nil))
         (tagbody
           (restart-bind
               (,@(mapcar (lambda (tag/clause)
                            (rs.with-tag/clause (tag/clause)
                              `(,name (lambda (&rest #3=#:temp)
                                        (setq #2# #3#)
                                        (go ,tag))
                                ,@(mapcan #'rs.transform-keyword-option options))))
                    tags/clauses))
             (return-from #1#
               ,(let ((form (macroexpand form)))
                  (if (and (consp form)
                           (member (car form) '(signal warn error cerror)))
                    (rs.expand-signalling-form form (mapcar #'car clauses))
                    form))))
           ,@(mapcan (lambda (tag/clause)
                       (rs.with-tag/clause (tag/clause)
                         `(,tag (return-from #1#
                                  (apply #'(lambda ,args ,@body) #2#)))))
               tags/clauses))))))
#+xyzzy (set-indent 'restart-case 1)

(defmacro with-simple-restart ((name fmt-control &rest fmt-arguments)
                               &body forms)
  "Evaluate FORMS with a retart NAME which simply returns nil and t."
  `(restart-case
       (progn ,@forms)
     (,name ()
      :report (lambda (stream)
                (format stream ,fmt-control ,@fmt-arguments))
      (values nil t))))
#+xyzzy (set-indent 'with-simple-restart 1)


;;;; Functions Use Active Restarts

(defun restart-applicable-p (restart condition)
  "Return true if restart is visible according to condition."
  (macrolet
      ((restart-is-associated-with-condition ()
         `(find restart (cdr (assoc condition *condition/restart-association*))))
       (restart-is-not-associated-at-all ()
         `(notany (lambda (association)
                    (member restart (cdr association)))
                *condition/restart-association*)))
    (and
     ;; check if its visible according to :test-function
     ;; NOTE: If test-function calls `compute-restarts` or `find-restarts`,
     ;; it invokes test-function once more and result in an infinite loop.
     ;; To prevent it, make the restart invisible while execution of its
     ;; test-function.
     (let ((*active-restarts* (remove restart *active-restarts*)))
       ;; NOTE: :test-function defaults to (constantly t)
       (funcall (restart-test-function restart) condition))
     ;; check association
     (or (null condition)
         (restart-is-associated-with-condition)
         (restart-is-not-associated-at-all)))))

(defun compute-restarts (&optional condition)
  "Return a list of restarts which are currently active and applicable."
  (remove-if-not #'(lambda (restart)
                     (restart-applicable-p restart condition))
        *active-restarts*))

(defun find-restart (identifier &optional condition)
  "Return a particular restart from current dynamic environment."
  (cond
   ((symbolp identifier)
    (find-if (lambda (restart)
               (and (eql (restart-name restart) identifier)
                    (restart-applicable-p restart condition)))
          *active-restarts*))
   ((restart-p identifier)
    (when (and (member identifier *active-restarts*)
               (restart-applicable-p identifier condition))
      identifier))
   (t (error "invalid restart designator: ~S" identifier))))

(define-condition restart-not-found (control-error)
  (designator condition)
  (:report (lambda (condition stream)
             (format stream "Could not find active restart ~S~@[ for condition ~S~]"
               (restart-not-found-designator condition)
               (restart-not-found-condition condition)))))
#+xyzzy (setf (get 'restart-not-found 'ed::lisp-indent-handler) t)

(defun invoke-restart (identifier &rest arguments)
  "Call the function associated with RESTART with ARGUMENTS."
  (apply (restart-function (or (find-restart identifier)
                               (error 'restart-not-found
                                      :designator identifier)))
         arguments))

(defun invoke-restart-interactively (identifier)
  (let ((restart (or (find-restart identifier)
                     (error 'restart-not-found
                            :designator identifier))))
    (apply #'invoke-restart
           restart
           (let ((it (restart-interactive-function restart)))
             (when it (funcall it))))))


;;;; Interface Functions for Standard Restart

(defun abort (&optional condition)
  (let ((r (find-restart 'abort condition)))
    (if r (invoke-restart r) (quit))))

(defun continue (&optional condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defun muffle-warning (&optional warning)
  (let ((restart (find-restart 'muffle-warning warning)))
    (if restart
      (invoke-restart restart)
      (error 'restart-not-found :designator 'muffle-warning :condition warning))))

(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun store-value (new-value &optional condition)
  (let ((r (find-restart 'store-value condition)))
    (when r (invoke-restart r new-value))))


;;;; Signaling Condition with Restarts

(defun rs.ensure-warning-designator (datum args)
  "Ensure DATUM and ARGS denotes a warning. If not, signal a `type-error`."
  (let ((warning (get 'warning 'si:structure-definition)))
    (macrolet ((TYPE-ERROR (actual expected)
                 `(error 'type-error :datum ,actual :expected-type ',expected)))
      (cond
       ;; condition object
       ((si:*structurep datum)
        (unless (si:*structure-subtypep
                 (si:*structure-definition datum)
                 warning)
          (TYPE-ERROR datum (or warning warning-type string)))
        (unless (null args)
          (TYPE-ERROR args null)))
       ;; condition type
       ((symbolp datum)
        (unless (let ((def (get datum 'si:structure-definition)))
                  (and def
                       (si:*structure-subtypep def warning)))
          (TYPE-ERROR datum (or warning warning-type string))))
       ;; warning message
       ((stringp datum) t)
       ;; other
       (t (TYPE-ERROR datum (or warning warning-type string)))))))

(defun warn (datum &rest args)
  (let ((warning (rs.make-condition datum args 'simple-warning)))
    (rs.ensure-warning-designator datum args)
    (restart-case
        (with-condition-restarts warning (list (find-restart 'muffle-warning))
          (si:*throw-error warning))
      (muffle-warning ()
        :report "この警告は無かったことに。"
        nil))))

(defun cerror (report-control datum &rest args)
  (let ((error (if (and (si:*structurep datum)
                        (si:*structure-subtypep (si:*structure-definition datum)
                                                (get 'condition 'si:structure-definition)))
                 datum
                 (rs.make-condition datum args 'simple-error))))
    (restart-case
        (with-condition-restarts error (list (find-restart 'continue))
          (si:*throw-error error))
      (continue ()
        :report (lambda (stream)
                  (apply #'format stream report-control args))))))


;;;; Function read-value

(defun read-value (prompt &optional predicate)
  (labels ((again (msg &rest args)
             (apply #'ed:message msg args)
             (read-value prompt predicate)))
    (let ((form (ed:read-sexp prompt)))
      (handler-case
          (let ((value (eval form)))
            (if (or (null predicate)
                    (funcall predicate value))
              value
              (again "不正な値です: ~S: ~S" value predicate)))
        (error (e)
          (again "エラー: ~A" e))))))

(provide "ansify/restart")
;;;; ansify/restart.l ends here.
