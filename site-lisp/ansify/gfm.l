;;; -*- mode: lisp; package: ansify.gfm -*-
;;;
;;; ansify/gfm.l --- Generic Function Modoki

;;; Code:


;;;; Package

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/function")
  (require "ansify/typespec+")
  (require "ansify/autoload-defs")
  (require "ansify/misc"))

(in-package :ansify)

(eval-when (:execute :compile-toplevel :load-toplevel)

  (shadowing-export
   '(;; class APIs and names
     find-class class-of class-name subclassp
     builtin-function structure-object structure-class defgeneric
     ;; GF/method
     generic-function-p ensure-generic-function defmethod
     add-method next-method-p call-next-method
     generic-function method ; type/class name
     ))

  (export-from :ansify.ext
               '(
                 ))
  )



;;;; utilities

(defun lambda-list-spec (lambda-list)
  (let ((n-req 0)
        (n-opt 0)
        (rest? nil)
        (key?  nil)
        (state '&required))
    (dolist (x lambda-list)
      (if (member x lambda-list-keywords)
        (setf state x)
        (case state
          (&required (incf n-req))
          (&optional (incf n-opt))
          (&rest     (setf rest? t))
          (&key      (setf rest? t key? t))
          (&aux)
          (otherwise (error 'invalid-lambda-list
                            :datum lambda-list)))))
    (values n-req n-opt rest? key?)))

(defun lambda-list-nargs (lambda-list)
  (multiple-value-bind (n-req n-opt rest?)
      (lambda-list-spec lambda-list)
    (values n-req (if rest? nil (+ n-req n-opt)))))

(defun lambda-list-congruent-p (list1 list2)
  (equal (multiple-value-list (lambda-list-spec list1))
         (multiple-value-list (lambda-list-spec list2))))



;;;; Class system modoki
;;
;; - find-class
;; - class-of
;; - class-name
;; - subclassp

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-%BIC (BIC out depth)
    (declare (ignore depth))
    (format out "#<built-in-class: ~A>"
            (%BIC-name BIC))))

(defstruct (%BIC
            (:copier nil)
            (:print-function print-%BIC))
  "Built-in-class object modoki."
  (name (error "initialization required: %BIC-name")
    ;; symbol naming the BIC
    :type symbol)
  (predicate (error "initialization required: %BIC-predicate")
    ;; predicate function to check an object is instance of the BIC or not
    :type function)
  (superclasses nil
    ;; list of direct superclass
    :type list)
  (subclasses nil
    ;; list of direct subclass
    :type list))

(defun new-BIC (name predicate &key superclasses subclasses)
  (make-%BIC :name name :predicate predicate
             :superclasses superclasses
             :subclasses subclasses))

;;;;; BIC namespace

(defparameter *BIC-namespace* nil
  "An alist holding all the bulit-in-classes.")

(defparameter *the-t-class* nil
  "The `t` class, root of everything.")

(defparameter *the-structure-object-class* nil
  "The `structure-object` class (being a built-in-class).")

(defun %find-BIC (name &optional (errorp t))
  (or (cdr (assoc name *BIC-namespace*))
      (when errorp
        (error "Unknown built-in-class: ~S" name))))

(defmacro %define-BIC (name (&rest superclasses) &key predicate type)
  "Define a BIC (built-in-class).

NAME is a symbol naming the BIC.

SUPERCLASSES are symbols naming other BICs which are direct superclass of
the BIC being defined.

PREDICATE (evaluates to a function) or TYPE (evaluates to typespec) can be
used to specify how to check an object is instance of this BIC or not.
PREDICATE takes priority over TYPE. If none of them are specified, symbol
NAME is used as a typespec."
  `(progn
     (let* ((superclasses
              (mapcar #'%find-BIC ',(or superclasses '(t))))
            (class
              (make-%BIC
               :name ',name
               :superclasses superclasses
               :predicate ,(or predicate `(typepred ,(or type `',name))))))
       (setf *BIC-namespace* (acons ',name class *BIC-namespace*))
       (dolist (super superclasses)
         (push class (%BIC-subclasses super)))
       class)))
(set-indent '%define-BIC 2)

(defun %initialize-BICs ()
  "Initialize BICs."

  ;; initialize BIC namespace
  (setf *the-t-class*   (new-BIC t (lambda (x) t))
        *BIC-namespace* (list (cons t *the-t-class*)))

  ;; each BICs
  (%define-BIC symbol ())
  (%define-BIC keyword (symbol))

  (%define-BIC sequence ())

  (%define-BIC list (sequence))
  (%define-BIC cons (list))
  (%define-BIC null (symbol list))

  (%define-BIC array ())
  (%define-BIC vector (sequence array))
  (%define-BIC string (vector))

  (%define-BIC number ())
  ;; FIXME: more number classes

  (%define-BIC function ())
  (%define-BIC generic-function (function) :predicate 'generic-function-p)
  (%define-BIC builtin-function (function) :predicate 'si:*builtin-function-p)
  ;; FIXME: macro-function being just a list...
  ;(%define-BIC macro-function (function))

  (%define-BIC character ())
  (%define-BIC stream ())
  (%define-BIC hash-table ())
  (%define-BIC package ())

  (%define-BIC structure-object () :predicate 'si:*structurep)
  (%define-BIC structure-class ()  :predicate 'si:*structure-definition-p)

  ;; FIXME: more BICs - specified by ANSI
  ;; FIXME: more BICs - xyzzy specific (buffer, regexp, marker,..)
  ;; FIXME: more BICs - extra (I want `buffer-stream` class)

  ;; finalize
  (setf *the-structure-object-class* (find-class 'structure-object))
  t)

;;;;; class APIs

(defun class-name (class)
  "Return symbol naming CLASS."
  (cond ((si:*structure-definition-p class)
          (si:*structure-definition-name class))
        ((%BIC-p class)
          (%BIC-name class))
        ((error 'type-error :datum class :expected-type 'class))))

(defun find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  "Return class object named NAME."
  (or (get name 'si:structure-definition)
      (%find-BIC name nil)
      (when errorp
        (error "Class not found: ~S" name))))

(defun class-of (object)
  "Return class object that OBJECT is instance of."
  (if (si:*structurep object)
    (si:*structure-definition object)
    (labels ((search (BIC)
               (dolist (subclass (%BIC-subclasses BIC) BIC)
                 (when (funcall (%BIC-predicate subclass) object)
                   (return (search subclass))))))
      (let ((direct-class (search *the-t-class*)))
        (when (eq direct-class *the-t-class*)
          ;; NOET: No object should be a direct instance of `t`.
          ;; If so, most likely appropriate BIC is missing.
          (warn "BIC missing? ~S" object))
        direct-class))))

(defun %BIC-precedence-list (BIC)
  "Return list of BICs sorted in order of precedence."
  ;; NOTE: Doing topological-sort in place, hopefully
  (let ((sorted  '())
        (visited '()))
    (labels ((visit (BIC)
               (unless (member BIC visited :test #'eq)
                 (push BIC visited)
                 (dolist (super (%BIC-superclasses BIC))
                   (visit super))
                 (push BIC sorted))))
      (visit BIC)
      sorted)))

(defun subclassp (class1 class2)
  "Return true if CLASS1 is subclass of CLASS2."
  (when (symbolp class1)
    (setf class1 (find-class class1)))
  (when (symbolp class2)
    (setf class2 (find-class class2)))
  (cond ((eq class1 class2) t)
        ((si:*structure-definition-p class1)
          (or (eq class2 *the-structure-object-class*)
              (eq class2 *the-t-class*)
              (and (si:*structure-definition-p class2)
                   (si:*structure-subtypep class1 class2))))
        ((%BIC-p class1)
          (and (%BIC-p class2)
               (member class2 (%BIC-precedence-list class1) :test #'eq)))))

(%initialize-BICs)


;;;; Generic function modoki
;;
;; Generic function is a function which holding bunch of methods and dispatch
;; to it on invocation depending on class of given arguments.
;;
;; The list of methods (and some other stuff) are stored in lexical environment
;; of the closure.

;; FIXME: invoke GF interactively

(defstruct gf-info
  (name (error "initialization required: gf-info-name")
    ;; function-name of the GF
    :type function-name) ;NOTE: undefined typespec
  (lambda-list (error "initialization required: gf-info-lambda-list")
    ;; gf-lambda-list
    :type lambda-list) ;NOTE: undefined typespec
  (methods nil
    ;; list of all method added to the GF
    :type list)
  (inherent-methods nil
    ;; list of method added by `defgeneric` form
    :type list)
  (documentation nil
    ;; GF's own docstring
    :type (or string null)))

(defmacro gf-info (gf)
  `(cdr (assoc 'GF-INFO (si:closure-variable ,gf))))

(defun generic-function-p (object)
  "Return true if OBJECT is a generic-function."
  (and (si:*closurep object)
       (gf-info object)))

;;;;; construction

;; [CLHS: 3.4.2 Generic Function Lambda Lists](http://www.lispworks.com/documentation/HyperSpec/Body/03_db.htm)
(defun check-gf-lambda-list (gf-lambda-list)
  (macrolet ((check (test)
               `(unless ,test (error 'invalid-lambda-list :datum gf-lambda-list))))
    (let ((state '&required)
          (required-exists-p nil))
      (dolist (x gf-lambda-list)
        (cond ((member x lambda-list-keywords)
                (check (member x '(&optional &rest &key &allow-other-keys)))
                (setf state x))
              ((case state
                 (&required
                   (check (symbolp x))
                   (setf required-exists-p t))
                 (&optional (check (or (symbolp x) (null (cdr x)))))
                 (&rest     (check (symbolp x)))
                 (&key      (check (or (symbolp x) (null (cdr x)))))
                 (&allow-other-keys (check nil))))))
      (check required-exists-p))))

(defun %make-generic-function (gf-name lambda-list documentation)
  (let ((GF-INFO
          ;; *** CAUTION *** don't change this name ***
          ;; Holding this variable in lexenv is vital (see macro `gf-info`)
          (make-gf-info :name gf-name
                        :lambda-list lambda-list
                        :documentation documentation)))
    (si:*set-function-name
     (lambda (&rest #1=#:args)
       (multiple-value-bind (nargs-min nargs-max)
           (lambda-list-nargs (gf-info-lambda-list GF-INFO))
         (%assert-list-length #1# nargs-min nargs-max)
         (%apply-method
          (or (appricable-method-list (gf-info-methods GF-INFO)
                                      #2=(subseq #1# 0 nargs-min)) ;just required portion
              (error "No appricable method: ~S: ~S" gf-name #2#))
          #1#)))
     gf-name)))

;; NOTE: missing args for `ensure-generic-function`
;; - argument-precedence-order
;; - declare
;; - generic-function-class
;; - method-class
;; - method-combination
(defun ensure-generic-function
        (function-name &key documentation lambda-list environment)
  "FUNCTION-NAME を総称関数にします。

関数が定義されていなければ総称関数として定義します。
（総称関数ではない）関数であればエラーになります。
既に総称関数であればオプションに応じて必要なら変更します。

オプション:
  :documentation 説明文（文字列）
      説明文を書いておきます。

  :lambda-list   ラムダリスト
      総称関数が受け付ける引数を指定するラムダリストです。
      形式は通常の関数と同じです。

  :environment   環境オブジェクト
      ※マクロの &environment でもらえるアレです
      今のところ使ってません。
"
  (check-gf-lambda-list lambda-list)
  (cond
   ;; A. undefined function
   ((not (fboundp function-name))
     (setf (fdefinition function-name) (%make-generic-function
                                        function-name lambda-list documentation))
     (let ((sym (canonicalize-function-name function-name)))
       (if documentation
         (setf (get sym 'lisp::function-documentation) documentation)
         (remprop sym 'lisp::function-documentation)))
     (fdefinition function-name))
   ;; B. defined as ordinary function, macro, or special operator
   ((not (generic-function-p (fdefinition function-name)))
     ;; NOTE: `fmakunbound` via restart might be useful
     ;; NOTE: CLHS defines that the error to be signalled is `type-error`
     (error "Already fbound to something other than GF: ~S" function-name))
   ;; C. already a GF
   ((generic-function-p (fdefinition function-name))
     (let ((symbol  (canonicalize-function-name function-name))
           (gf-info (gf-info (fdefinition function-name))))
       ;; documentation
       (unless (equal documentation
                      (gf-info-documentation gf-info))
         (setf (gf-info-documentation gf-info) documentation)
         (if documentation
           (setf (get symbol 'lisp::function-documentation) documentation)
           (remprop symbol 'lisp::function-documentation)))
       ;; lambda-list
       (unless (equal lambda-list
                      (gf-info-lambda-list gf-info))
         (if (every (lambda (method)
                      (lambda-list-congruent-p lambda-list
                                               (method-lambda-list method)))
                    (gf-info-methods gf-info))
           (setf (gf-info-lambda-list gf-info) lambda-list)
           (error "New lambda-list is not congruent with existing method: ~S"
                  function-name)))
       (fdefinition function-name)))))
(setf (get 'ensure-generic-function 'ed:lisp-indent-hook) 1)

;;;;; macro `defgeneric`

(defun %parse-defgeneric-options (options name)
  "Classify OPTIONS given to `defgeneric`.
=> documentation  -- documentation string
=> method-descs   -- list of (METHOD-LAMBDA-LIST . BODY)"
  (let ((doc nil)
        (method-descs '()))
    (dolist (opt options)
      (case (car opt)
        (:documentation
          (when doc (error "More than one documentations: (defgeneric ~S ..)" name))
          (setf doc (second opt)))
        (:method
          ;; (:method METHOD-LAMBDA-LIST . BODY)
          (push (cdr opt) method-descs))
        (otherwise
          (error "Unknown (or unsupported) option: ~S: (defgeneric ~S ..)"
                 (car opt) name))))
    (values doc (nreverse method-descs))))

(defmacro defgeneric (name gf-lambda-list &rest options)
  "総称関数 NAME を定義します。

NAME は関数として未定義か、定義済みの総称関数でなければなりません。
（通常の関数であればエラーになります。）

オプションは (:OPTION-TYPE ...) というリストで指定します。指定できる
OPTION-TYPE は以下の通りです。

  (:documentation 説明文)

;; 以下未実装
  (:method METHOD-LAMBDA-LIST &body BODY)
      総称関数を定義する時に、ついでにメソッドを追加します。
      このオプションで定義されたメソッドは総称関数が再定義されると
      消えてしまいます。
    "
  (multiple-value-bind (doc method-descs)
      (%parse-defgeneric-options options name)
    `(progn
       ;; remove methods added by previous `defgeneric`
       (when (and (fboundp ',name)
                  (generic-function-p (fdefinition ',name)))
         (let ((gf (fdefinition ',name)))
           (dolist (m (gf-info-inherent-methods (gf-info gf)))
             (remove-method gf m))
           (setf (gf-info-inherent-methods (gf-info gf)) nil)))
       ;; do `ensure-generic-function`
       (ensure-generic-function ',name
         :lambda-list ',gf-lambda-list
         :documentation ,doc)
       ;; add methods specified by `:method` options
       (let* ((gf (fdefinition ',name))
              (gf-info (gf-info gf)))
         ,@(mapcar (lambda (method-desc)
                     `(let ((method ,(gen-make-method-form name (car method-desc) (cdr method-desc))))
                        (add-method gf method)
                        (push method (gf-info-inherent-methods gf-info))))
                   method-descs)
         gf)
       )))
(set-indent 'defgeneric 'defun)


;;;; Method modoki
;;
;; Method is an object holding piece of code to executed, but method itself
;; is not a function. It lives in a GF's method list and invoked when GF got
;; applied to a set of arguments the method is specialized to.

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-method (m out depth)
    (declare (ignore depth))
    (format out "#<method: ~S ~S #{~8S}>"
            (method-name m)
            (mapcar (lambda (spec)
                      (case (car spec)
                        (class (cdr spec))
                        (eql   (list 'eql (cdr spec)))))
                    (method-specializers m))
            (si:address-of m))))

(defstruct (method
            (:print-function print-method))
  (name (error "initialization required: method-name")
    ;; name of GF the method lives in
    :type function-name) ;NOTE: undefined typespec
  (lambda-list (error "initialization required: method-lambda-list")
    ;; method's specialized lambda-list
    :type lambda-list) ;NOTE: undefined typespec
  (specializers (error "initialization required: method-specializers")
    ;; list of specializers (in canonicalized form)
    :type list)
  (thunk (error "initialization required: method-thunk")
    ;; function executes what the method does
    :type function)
  (documentation nil
    ;; docstring of the method
    :type (or string null)))

;;;;; construction

(defun %gen-specializer-form (spec)
  "Return a form evaluates to canonical form of specializer.
SPEC must be a specializer in form appear in method-lambda-list."
  ;; (eql FORM) --> (cons 'eql FORM)          --> (eql . OBJECT)
  ;; CLASS-NAME --> (cons 'class 'CLASS-NAME) --> (class . CLASS-NAME)
  ;; #<CLASS>   --> (cons 'class 'CLASS-NAME) --> (class . CLASS-NAME)
  (cond
   ((and (listp spec) (eq (car spec) 'eql))
     `(cons 'eql ,(second spec)))
   ((symbolp spec)
     `(cons 'class ',spec))
   ((or (%BIC-p spec) (si:*structure-definition-p spec))
     `(cons 'class ',(class-name spec)))
   (t
     (error "Invalid specializer: ~S" spec))))

(defun %parse-method-lambda-list (method-lambda-list)
  "Parse METHOD-LAMBDA-LIST.
=> LAMBDA-LIST (ordinary one)
=> SPECIALIZERS"
  (let ((required-vars '())
        (specializers  '()))
    (labels ((%return (rest)
               (return-from %parse-method-lambda-list
                 (values (append (nreverse required-vars) rest)
                         (nreverse specializers)))))
      ;; loop over just required portion and collect parameter variables
      ;; and specializers separately
      (do ((rest method-lambda-list (cdr rest)))
          ((null rest))
        (let ((x (first rest)))
          (cond ((member x lambda-list-keywords)
                  (%return rest))
                ((consp x)
                  (push (first x)  required-vars)
                  (push (second x) specializers))
                ((symbolp x)
                  (push x required-vars)
                  (push t specializers))
                (t
                  (error 'invalid-lambda-list :datum method-lambda-list)))))
      (%return nil))))

(defparameter *appricable-method-list* nil)

(defun gen-make-method-form (name method-lambda-list body)
  (multiple-value-bind (lambda-list specializers)
      (%parse-method-lambda-list method-lambda-list)
    (multiple-value-bind (body decls doc intr)
        (parse-body body)
      `(make-method
        :name ',name
        :specializers (list ,@(mapcar #'%gen-specializer-form specializers))
        :lambda-list ',method-lambda-list
        :documentation ,doc
        :thunk (si:*set-function-name
                (lambda (&rest #0=#:method-args)
                  ,@decls
                  ,@(when intr `(,intr)) ;NOTE: interactive invocation not supported on GF side
                  (block ,(cond ((setf-function-name-p name) (second name))
                                (t name))
                    (destructuring-bind ,lambda-list #0#
                      (labels ((next-method-p ()
                                 (and *appricable-method-list* t))
                               (call-next-method (&rest #1=#:next-args)
                                 (unless *appricable-method-list*
                                   (error "No next method: ~S" ',name))
                                 (%apply-method *appricable-method-list*
                                                (or #0# #1#))))
                        ,@body))))
                ',name)))))

;;;;; add/remove method to/from GF

(defun %methods-agree-p (method1 method2)
  "Return true if two methods same specializer."
  ;; NOTE: ANSI inconfirmance - no support for qualifier
  (equal (method-specializers method1)
         (method-specializers method2)))

;; [CLHS: Standard Generic Function ADD-METHOD]: http://www.lispworks.com/documentation/HyperSpec/Body/f_add_me.htm
(defun add-method (generic-function new-method)
  "Add method NEW-METHOD to GENERIC-FUNCTION.
If there's a method which has same specializer with NEW-METHOD on the GF,
it is replaced with NEW-METHOD."
  (let* ((gf-info (gf-info generic-function))
         (old-one (find-if (lambda (existing-method)
                             (%methods-agree-p existing-method new-method))
                           (gf-info-methods gf-info))))
    (when old-one
      (remove-method generic-function old-one))
    (push new-method (gf-info-methods gf-info))
    generic-function))

;; [CLHS: Standard Generic Function REMOVE-METHOD]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rm_met.htm
(defun remove-method (generic-function method)
  (let ((gf-info (gf-info generic-function)))
    (unless (member method (gf-info-methods gf-info))
      (error "Not a method added to the GF: ~S: ~S" method generic-function))
    (setf (gf-info-methods gf-info) (delete method (gf-info-methods gf-info))
          (gf-info-inherent-methods gf-info) (delete method (gf-info-inherent-methods gf-info)))
    generic-function))



;;;;; macro `defmethod`

(defun %method-lambda-list-to-gf-lambda-list (method-lambda-list)
  (let ((gf-lambda-list '())
        (state '&required))
    (labels ((symbol-or-car (x)
               (if (symbolp x) x (car x))))
      (dolist (x method-lambda-list)
        (cond ((member x lambda-list-keywords)
                (push x gf-lambda-list)
                (setf state x))
              ((case state
                 (&required (push (symbol-or-car x) gf-lambda-list))
                 (&optional (push (symbol-or-car x) gf-lambda-list))
                 (&rest     (push x gf-lambda-list))
                 (&key
                   (push (cond ((and (consp x) (consp (car x))) ; ((KEYWORD VAR) INIT SP)
                                 (second (car x)))
                               ((consp x) (car x))
                               (t x))
                         gf-lambda-list))
                 ((&allow-other-keys &aux))
                 (t (error 'invalid-lambda-list :datum method-lambda-list))))))
      (nreverse gf-lambda-list))))

(defmacro defmethod (&environment env
                     name method-lambda-list &body body)
  "総称関数 NAME にメソッドを定義（追加）します。

引数:
  NAME : メソッドを追加する総称関数の名前
  METHOD-LAMBDA-LIST : 特定のクラスなどに特定化するラムダリスト
  BODY : 処理本体

定義されたメソッドは METHOD-LAMBDA-LIST で特定化したクラスのオブジェクト
を引数に総称関数が呼び出された時に実行されます。
METHOD-LAMBDA-lIST は基本的には通常の関数で指定するラムダリストと同様
ですが、必須引数を特定のクラスに特定化することができます。

    (defmethod GF-NAME ((x string) (y list) &optional z)
      ...)

詳しくは... " ;; FIXME: ...書くの？
  (multiple-value-bind (*body decls doc intr)
      (parse-body body)
    (let ((gf-lambda-list
            (%method-lambda-list-to-gf-lambda-list method-lambda-list)))
      `(progn
         (unless (and (fboundp ',name)
                      (generic-function-p (fdefinition ',name)))
           (ensure-generic-function ',name
             :lambda-list ',gf-lambda-list
             :documentation ,doc
             :environment ',env))
         (let ((gf-info (gf-info (fdefinition ',name))))
           (unless (lambda-list-congruent-p ',method-lambda-list
                                            (gf-info-lambda-list gf-info))
             (error "Method lambda list incongruent: ~S: ~S"
                    ',name ',method-lambda-list))
           (let ((method ,(gen-make-method-form name method-lambda-list body)))
             (add-method (fdefinition ',name) method)
             method))))))
(setf (get 'defmethod 'ed:lisp-indent-hook) 'defun)


;;;; Applying GF/method

(defun %method-appricable-p (method args)
  (every (lambda (spec arg)
           (let ((spec-type  (car spec))
                 (spec-datum (cdr spec))
                 (arg-class  (class-of arg)))
             (case spec-type
               (eql   (eql arg spec-datum))
               (class (subclassp arg-class (find-class spec-datum)))
               (t     (error "Invalid specializer: ~S" spec)))))
         (method-specializers method)
         args))

(defun %specializer-more-specific-p (spec1 spec2)
  ;; NOTE: Assuming SPEC1 and SPEC2 are different
  (cond ((eq (car spec1) 'eql) t)
        ((eq (car spec2) 'eql) nil)
        ((subclassp (cdr spec1) (cdr spec2)) t)))

(defun %method-more-specific-p (method1 method2)
  (do ((specs1 (method-specializers method1) (cdr specs1))
       (specs2 (method-specializers method2) (cdr specs2)))
      ((null specs1) nil)
    (let ((spec1 (car specs1))
          (spec2 (car specs2)))
      (cond ((%specializer-more-specific-p spec1 spec2) (return t))
            ((%specializer-more-specific-p spec2 spec1) (return nil))))))

;; FIXME: cache list of appricable-methods
(defun appricable-method-list (all-methods args)
  (sort (remove-if-not (lambda (method)
                         (%method-appricable-p method args))
                       all-methods)
        #'%method-more-specific-p))

(defun %apply-method (method-list args)
  (let ((*appricable-method-list* (cdr method-list)))
    (apply (method-thunk (car method-list)) args)))

;;;; next-method dummies

(defun next-method-p ()
  (error "Not in a method: ~S" 'next-method-p))

(defun call-next-method (&rest args)
  (error "Not in a method: ~S" 'call-next-method))


(provide "ansify/gfm")
;;; ansify/gfm.l ends here.
