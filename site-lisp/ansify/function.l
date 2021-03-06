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
                      function-lambda-expression
                      fboundp
                      fmakunbound
                      trace untrace
                      ))
  (export-from :ansify.ext
               '(#:function-info #:function-name
                 #:fwrapper #:find-fwrapper #:define-fwrapper #:fwrap #:funwrap
                 #:call-next-function #:*this-function*
                 ))
  )


;;;; Accessor function-info

(defparameter +function-info-key+ 'function-info)

(lisp:defun %fn-info-table (fn &optional create)
  (let* ((body (cddr (si:closure-body fn)))
         (decl (when (operator-form-p (first body) 'declare)
                 (first body)))
         (table (dolist (x decl)
                  (when (operator-form-p x +function-info-key+)
                    (return x)))))
    (cond (table table)
          (create
            (let ((entry (list +function-info-key+)))
              (if decl
                (setf (cdr (last decl)) (list entry))
                (setf (cddr (si:closure-body fn))
                      ;; NOTE: (special #:dummy) workaround
                      ;; When a lexical closure has declarations at beginning of
                      ;; its' body forms, xyzzy generally ignores them. But when
                      ;; a) there's no declaration with `special` and b) environment
                      ;; of function call contains binding for special variable,
                      ;; they aren't ignored and result in an error (try to eval
                      ;; the `declare` form as usual functio call).
                      ;; Insert dummy special declaration to prevent this.
                      (cons `(declare ,entry (special #:dummy))
                            (cddr (si:closure-body fn)))))
              entry)))))

(lisp:defun function-info (func key)
  "関数 FUNCTION に保存されたデータを取得します。
データを保存するのには setf を使います。

使用例:
  ;; 保存する
  (setf (function-info #'some-fn 'some-key) \"some data\")

  ;; 取得する
  (function-info #'some-fn 'some-key)
  => \"some data\""
  (cdr (assoc key (cdr (%fn-info-table func)))))

(defsetf function-info (func key) (new-value)
  `(let ((table (%fn-info-table ,func t)))
     (setf (cdr table) (acons ,key ,new-value (cdr table)))
     ,new-value))


;;;; function name 

(lisp:defun setf-function-name-p (x)
  (and (consp x)
       (eql (first x) 'setf)
       (symbolp (second x))
       (null (cddr x))))

(lisp:defun get-setf-function-symbol (name)
  (unless (setf-function-name-p name)
    (error "不正な関数名です: ~S" name))
  (let ((accessor (second name)))
    (intern (format nil "(setf ~A)" accessor)
            (symbol-package accessor))))

(lisp:defun canonicalize-function-name (name)
  (if (symbolp name) name
    (get-setf-function-symbol name)))

(deftype function-name ()
  `(or symbol
       (cons (eql setf) (cons symbol null))))

;; NOTE: When getter for `si:*set-function-name` got implemented, use it.
(lisp:defun function-name (fn)
  "関数の名前であるシンボル等を探して、見つかればそれを、無名関数だったり
関数名を見つけられない場合などには nil を返します。

\(setf READER) のようなヘンな名前を返す事もあるので気を付けるべし。"
  (labels ((fn-repr (fn)
             (let ((*package* (find-package :keyword)))
               (prin1-to-string fn))))
    (cond ((si:*closurep fn)
            (or (function-info fn 'function-name)
                (setf (function-info fn 'function-name)
                      (if (compiled-function-p fn)
                        (when (ed:string-match "^#<lexical-closure: \\(.+\\)>$"
                                               (fn-repr fn))
                          (read-from-string (ed:match-string 1)))
                        (dolist (form (cddr (si:closure-body fn)))
                          (when (operator-form-p form 'block)
                            (return (second form))))))))
          ((si:*builtin-function-p fn)
            (when (ed:string-match "^#<function: \\(.+\\)>$" (fn-repr fn))
              (or (find-symbol (ed:match-string 1) :editor)
                  (find-symbol (ed:match-string 1) :system))))
          (t
            (error 'type-error :datum fn :expected-type 'function)))))


;;;; function-lambda-expression
;;
;; [CLHS: Function FUNCTION-LAMBDA-EXPRESSION]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fn_lam.htm
(lisp:defun function-lambda-expression (function)
  "関数の 1) ラムダ式 2) 環境の有無 3) 名前を返します。

1) ラムダ式 はただのラムダ式です。わからん場合は nil を返します。
2) 環境の有無 は関数が持っているレキシカル環境に何かあるかどうかです。
空のレキシカル環境であれば nil を返しますが、それ以外（わからない場合
を含む）は non-nil を返します。
3) 名前 は関数の名前です。通常はシンボルですがたまに違うものだったり
します。わからなければ nil を返します。"
  (labels ((null-closure-p (fn)
             (and (null (si:closure-variable fn))
                  (null (si:closure-frame fn))
                  (null (si:closure-function fn))
                  t)))
    (cond ((si:*builtin-function-p function)
            (values nil t (function-name function)))
          ((and (si:*closurep function)
                (compiled-function-p function))
            (values (function-info function 'lambda-expression)
                    (not (null-closure-p function))
                    (function-name function)))
          ((si:*closurep function)
            (values (or (function-info function 'lambda-expression)
                        (si:closure-body function))
                    (not (null-closure-p function))
                    (function-name function)))
          (t
            (error 'type-error :datum function :expected-type 'function)))))


;;;; fwrapper
;;
;; FIXME: documnetation of fwrapper

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
  "NAME という名前の fwrapper を探して返します。
見つからなければエラーになります。"
  (cond ((symbolp name)
          (or (find name *fwrapper-list* :key #'fwrapper-name)
              (error "Undefined fwrapper: ~S" name)))
        ((fwrapper-p name) name)
        (t (error 'type-error :datum name :expected-type '(or symbol fwrapper)))))

;;;;; define-fwrapper

(eval-when (:execute :compile-toplevel :load-toplevel)
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
      '(fwrapper ,name))))


(defmacro define-fwrapper (name fw-lambda-list &body body)
  "関数をラップして処理を追加する fwrapper を定義します。

引数:
  NAME           : fwrapper の名前となるシンボル
  FW-LAMBDA-LIST : ラムダリスト（関数と同じ）
  BODY           : 関数をラップする処理本体

fwrapper で関数をラップすると、その関数が呼び出されたときに元の関数の処理
の代わりに fwrapper の BODY が実行されるようになります。
fwrapper の BODY では、関数 call-next-function を呼び出すことで元の関数の
処理を実行する事ができます。その際に call-next-function へ引数を渡すと、
元の関数には渡した引数が与えられます。引数なしで call-next-function を呼び
出した場合は、fwrapper が受け取った引数全てが与えられます。

ひとつの関数を複数の fwrapper でラップすることもできます。その場合後からラ
ップした方が最初に実行され、call-next-function を呼び出すと次の fwrap され
た関数が実行されるようになります。

使用例:
  ;; 対象の関数
  (defun add (x y) (+ x y))

  ;; fwrapper を定義
  (define-fwrapper string-as-integer (&rest args)
    (handler-case
        ;; とりあえず引数全てそのまま渡してみて
        (call-next-function)
      (type-error ()
        ;; ダメだったら parse-integer して渡す
        (apply #'call-next-function
               (mapcar #'parse-integer args)))))

  ;; 元の関数に fwrapper を適用
  (fwrap #'add 'string-as-integer)

  ;; 文字列で呼び出す
  (add \"3\" \"8\")
  => 11"
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

(defparameter *this-function* nil
  "fwrapper の本体を実行中、ラップされている関数を持っています。")

(lisp:defun call-next-function (&rest next-args)
  "fwrapper の中から、ラップされた元の関数を呼び出します。

引数が与えられると、与えられた引数を元の関数へ渡します。引数が与えられ
なかった場合は、fwrapper の呼び出しに与えられた引数をそのまま元の関数
へ渡します。"
  ;; NOTE: Actual call-next-function is defined for each fwrapper locally.
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
                               ,@(second primary)))))
            (function-info-table
              (%fn-info-table fn)))
        (setf (second closure) `(&rest initial-args)
              (cddr closure)   `((declare ,function-info-table)
                                 (let ((*this-function* ,fn))
                                   (apply ,(first next-function-list)
                                          (list ,@(rest next-function-list))
                                          initial-args))))
        #+nil
        (setf (second closure) `(&rest initial-args
                                 ;; NOTE: Keep track of function-info of fwrapped/primary function
                                 ;; We're replacing where function-info table living, and
                                 ;; loosing access to it means the fwrapped function doesn't
                                 ;; have primary-function backup and unable to funwrap anymore.
                                 &aux (,+fn-info-marker+ ,(%fn-info-table fn)))
              (cddr closure)   `((let ((*this-function* ,fn))
                                   (apply ,(first next-function-list)
                                          (list ,@(rest next-function-list))
                                          initial-args))))
        ;; FIXME: compile the fwrapped function?
        ))))


(lisp:defun fwrap (function fwrapper &optional (indicator (gensym "fwrap-")))
  "関数 FUNCTION を FWRAPPER でラップします。

引数:
  FUNCTION  : ラップされる関数（シンボルでも関数でも）
  FWRAPPER  : fwrapper の名前（シンボル）
  INDICATOR : インジケータ（何でもあり。普通はシンボル。省略可）

関数 FUNCTION を FWRAPPER でラップすると、その関数が呼び出された際に
本来の処理の代わりに FWRAPPER が実行されるようになります。
詳細は define-fwrapper の方を参照。

INDICATOR を指定しておくと、同じ INDICATOR を指定して別の fwrapper で
ラップしたり、ラップを外したりできます。"
  ;; FIXME: check the args
  (%ensure-primary-function-backup function)
  (setf (getf (function-info function 'active-fwrappers) indicator) (find-fwrapper fwrapper))
  (%finalize-fwrapped-function function)
  function)

(lisp:defun funwrap (function &optional indicator)
  "関数 FUNCTION をラップしている fwrapper をはがします。

引数:
  FUNCTION  : fwrap されている関数
  INDICATOR : インジケータ

INDICATOR が指定されればその INDICATOR を指定してラップした fwrapper を、
指定されなければ全ての fwrapper をはがします。"
  (if indicator
    (remf (function-info function 'active-fwrappers) indicator)
    (setf (function-info function 'active-fwrappers) nil))
  (%finalize-fwrapped-function function)
  function)

;;;; trace

(defparameter *trace-depth* nil)

(define-fwrapper trace (&rest args)
  ;; FIXME: Printing to *standard-output* (should be *trace-output* according to ANSI)
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
             ;; FIXME: Extract "check that X is a function-name" bits
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



;;;; Macro defun

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro defun (name (&rest lambda-list) &body body)
    (labels ((check-redefinition (sym name)
               `(when (and (fboundp ',sym)
                           *warn-on-redefine-function*)
                  (warn 'redefining-function :name ',name :type 'function))))
      (cond ((symbolp name)
             `(progn
                ,(check-redefinition name name)
                (lisp:defun ,name ,lambda-list ,@body)
                (setf (function-info #',name 'lambda-expression)
                      '(lambda ,lambda-list ,@(copy-tree body)))
                ',name))
            ((consp name)
             (let ((sym    (get-setf-function-symbol name))
                   (reader (second name)))
               (multiple-value-bind (nbody decls doc intr)
                   (parse-body body)
                 `(progn
                    ,(check-redefinition sym name)
                    (setf (fdefinition ',name)
                          (si:*set-function-name
                           (lambda ,lambda-list ,@decls ,@(when intr `(,intr))
                             (block ,reader ,@nbody))
                           ',name))
                    ,(if doc
                       `(setf (get ',sym 'lisp::function-documentation) ,doc)
                       `(remprop ',sym 'lisp::function-documentation))
                    (setf (function-info (fdefinition ',name) 'lambda-expression)
                          '(lambda ,lambda-list ,@(copy-tree body)))
                    ',name))))))))
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
