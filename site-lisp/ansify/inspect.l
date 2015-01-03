;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/inspect.l
;;;
;;; Author:     bowbow99 <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:


;;;; Package

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base")
  (require "ansify/gfm"))

(in-package :ansify)

(eval-when (:load-toplevel :compile-toplevel :execute)

  (shadowing-export '(inspect describe describe-object
                      ))

  (export-from :ansify.ext
               '(#:inspect-object
                 ))

  ) ; end of `eval-when`


;;;; Utilities

(defun %prin* (x &optional (out *standard-output*))
  (if (si:*structurep x)
    (princ x out)
    (prin1 x out)))

(defun %prin*-to-string (x)
  (with-output-to-string (s) (%prin* x s)))

(defconstant +undefined+ '#:undefined)

(defun maybe-undef (acc)
  (lambda (obj)
    (handler-case
        (funcall acc obj)
      (error (e)
        +undefined+))))



;;;; Inspect Protocol

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-inspectable (o s n)
    (declare (ignore n))
    (cond (*print-readably*
            (print-structure-readably o s))
          (t
            (print-unreadable-object (o s :type t :identity t)
              )))))

(defstruct (inspectable
            (:print-function print-inspectable))
  name
  label
  reader
  writer
  undefined-label
  type
  )

(defun %inspectable (name label type &key reader writer accessor (undefined-label "[undefined]"))
  (when accessor
    (setf reader accessor  writer accessor))
  (make-inspectable :name name  :label label  :reader reader  :writer writer
                    :undefined-label undefined-label  :type type))

(defgeneric inspect-object (object)
  (:documentation
   "OBJECT の調べられるトコを inspectable オブジェクトのリストで返す。"))

(defmacro definspect (class &rest specs)
  `(defmethod inspect-object ((,class ,class))
     (list ,@(mapcar (lambda (spec)
                       (apply (lambda (name label type &rest args)
                                `(%inspectable ',name ,label ',type ,@args))
                              spec))
                     specs))))
(setf (get 'definspect 'ed:lisp-indent-hook) 'defun
      (get 'definspect 'ed::lisp-indent-clause) 2)

(definspect symbol
  (name         "シンボル名"      string    :reader 'symbol-name)
  (home-package "所属パッケージ"  package   :reader 'symbol-package)
  (fdefinition  "関数定義"        function  :accessor (maybe-undef 'fdefinition))
  (value        "変数としての値"  t         :accessor (maybe-undef 'symbol-value))
  (symbol-plist "プロパティ"      (plist-of symbol t)
    :reader 'symbol-plist))

;; sequence
;;  list
(definspect list
  (length    "長さ" number  :reader 'list-length)
  (elements  "要素" (list-of t) :reader 'identity))


;;   cons
;;   null

;;  array
;;   vector
;;    string

;; number

;; function
;;  generic-function
;;  builtin-function

;; character
(definspect character
  (char-code "コード" number :reader 'char-code))



;; stream

(macrolet ((def (name repr)
             `(defun ,(mksym "~A-p" name) (x)
                (let ((name (prin1-to-string x)))
                  (and (streamp x)
                       (> (length name) ,(length repr))
                       (string= ,repr name :end2 ,(length repr)))))))
  (def file-input-stream "#<file-input stream: ")
  (def file-output-stream "#<file-output stream: ")
  (def file-io-stream "#<file-io stream: "))

(defun file-stream-pathname (stream)
  (when (ed:string-match "^#<file-\\(?:input\\|output\\|io\\) stream: \\(.*\\)>"
                         (prin1-to-string stream))
    (let ((pathname (ed:match-string 1)))
      (if (string= pathname "-")
        nil
        pathname))))

(defparameter +stream-type/predicate-alist+
  '((file-input-stream-p . file-input-stream)
    (file-output-stream-p . file-output-stream)
    (file-io-stream-p . file-io-stream)
    (string-input-stream-p . string-input-stream)
    (string-output-stream-p . string-output-stream)
    (synonym-stream-p . synonym-stream)
    (broadcast-stream-p . broadcast-stream)
    (echo-stream-p . echo-stream)
    (two-way-stream-p . two-way-stream)
    (ed:buffer-stream-p . ed:buffer-stream)
    (ed:status-window-stream-p . ed::status-window-stream)
    (ed:debug-output-stream-p . ed::debug-output-stream)
    (ed:socket-stream-p . ed:socket-stream)
    (ed:general-input-stream-p . ed::general-input-stream)
    (ed:general-output-stream-p . ed::general-output-stream)
    ;; wstreams-stream
    ))

(defun stream-type-of (stream)
  (check-type stream stream)
  (or (cdr (assoc stream +stream-type/predicate-alist+
                  :test (lambda (x y) (funcall y x))))
      'stream))

(defun stream-direction (stream)
  (let ((in  (input-stream-p stream))
        (out (output-stream-p stream)))
    (cond ((and in out) "input/output")
          (in "input")
          (out "output"))))

(defun socket-stream-local-info (s)
  (format nil "~A:~A (~A)"
          (ed:socket-stream-local-address s)
          (ed:socket-stream-local-port s)
          (ed:socket-stream-local-name s)))

(defun socket-stream-peer-info (s)
  (format nil "~A:~A (~A)"
          (ed:socket-stream-peer-address s)
          (ed:socket-stream-peer-port s)
          (ed:socket-stream-peer-name s)))

(defun socket-stream-inspectables (s)
  (list (%inspectable 'local "ローカル側" 'text :reader 'socket-stream-local-info)
        (%inspectable 'peer "ピア側" 'text :reader 'socket-stream-peer-info)
        (%inspectable 'timeout "タイムアウト" 'number
                      :reader 'ed:socket-stream-get-timeout
                      :writer 'ed:socket-stream-set-timeout)
        (%inspectable 'ssl-p "SSL" 'boolean :reader 'ed:socket-stream-ssl-p)))


(defmethod inspect-object ((s stream))
  (let ((type (stream-type-of s)))
    (list*
     (%inspectable 'stream-type "ストリームの種類" 'text :reader (constantly (string type)))
     (%inspectable 'direction "入出力" 'text :reader 'stream-direction)
     (%inspectable 'open-p "開いているか？" 'boolean :reader 'open-stream-p)
     (%inspectable 'encoding "エンコーディング" 't :reader 'ed:stream-encoding)
     (case type
       ((file-input-stream file-output-stream file-io-stream)
        (list (%inspectable 'file "ファイル" 'text :reader 'file-stream-pathname)))
       ((string-input-stream-p string-output-stream-p))
       (synonym-stream
        (list (%inspectable 'symbol "対象のシンボル" 'symbol :reader 'synonym-stream-symbol)))
       (broadcast-stream
        (list (%inspectable 'destinations "出力先ストリーム" '(list-of stream)
                            :reader 'broadcast-stream-streams)))
       (echo-stream
        (list (%inspectable 'source "入力元ストリーム" 'stream :reader 'echo-stream-input-stream)
              (%inspectable 'destination "出力先ストリーム" 'stream :reader 'echo-stream-output-stream)))
       (two-way-stream
        (list (%inspectable 'input "入力元ストリーム" 'stream :reader 'two-way-stream-input-stream)
              (%inspectable 'output "出力先ストリーム" 'stream :reader 'two-way-stream-output-stream)))
       (ed:buffer-stream
        (list (%inspectable 'buffer "バッファ" 'buffer :reader 'ed:buffer-stream-buffer)
              (%inspectable 'point "現在の位置" 'number
                            :reader 'ed:buffer-stream-point
                            :writer 'ed:buffer-stream-set-point)))
       (ed::status-window-stream)
       (ed::debug-output-stream)
       (ed:socket-stream
        (socket-stream-inspectables s))
       (ed::general-input-stream)
       (ed::general-output-stream)
       ))))

;; FIXME: hash-table

(defun %hash-table-entries-to-plist (table)
  (let (plist)
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(definspect hash-table
  (count "エントリ数" number :reader 'hash-table-count)
  (size  "サイズ" number :reader 'hash-table-size)
  (test  "比較関数" symbol :reader 'hash-table-test)
  ;; hash-table-rehash-size
  ;; hash-table-rehash-threshold
  (entries "エントリ" (plist-of t t)
    :reader '%hash-table-entries-to-plist))

;; package
(definspect package
  (name         "パッケージ名" string
    :reader 'package-name)
  (nicknames    "ニックネーム" (list-of string)
    :accessor 'package-nicknames)
  (using        "use-package されているパッケージ" (list-of package)
    :reader 'package-use-list)
  (used-by      "use-package しているパッケージ" (list-of package)
    :reader 'package-used-by-list)
  (documentation "説明文" docstring
    :reader (lambda (x) (or (documentation package t) +undefined+))
    :undefined-label "[無し]")
  ;external-symbols
  ;internal-symbols
  )

;; structure-object
(defun %structure-slot-and-values (obj)
  (let ((def (si:*structure-definition obj))
        (slot-and-values))
    (dotimes (i (si:*structure-definition-nslots def))
      (push (si:*structure-definition-slot-description def i) slot-and-values)
      (push (si:*index-slot-value obj i) slot-and-values))
    (nreverse slot-and-values)))

(definspect structure-object
  (class "クラス" structure-class :reader 'si:*structure-definition)
  (slot-values "スロットとその値" (plist-of text t) :reader '%structure-slot-and-values))

;; structure-class

(defun structure-slot-description-text (def i)
  (multiple-value-bind (name init type read-only-p)
      (si:*structure-definition-slot-description def i)
    (let* ((init         (if (gensymp init) nil init))
           (type         (if (gensymp type) t   type))
           (print-type-p (not (eq type t)))
           (read-only-p  (if (gensymp read-only-p) nil read-only-p)))
    ;; let's do the job
    (with-output-to-string (s)
      (format s "~A~16,4T= ~S" name init)
      (when (or print-type-p read-only-p) (format s "~24,4T{"))
      (when print-type-p
        (format s "type = ~S" type)
        (when read-only-p (format s ", ")))
      (when read-only-p
        (format s "read-only = t"))
      (when (or print-type-p read-only-p) (format s "}"))
      ))))

(defun structure-definition-slot-descriptions (def)
  (let ((descs '()))
    (dotimes (i (si:*structure-definition-nslots def))
      (push (structure-slot-description-text def i) descs))
    (nreverse descs)))

(defun structure-definition-documentation (def)
  (documentation (si:*structure-definition-name def) 'structure))

(definspect structure-class
  (name "名前" symbol :reader 'si:*structure-definition-name)
  (type "構造体の種類" symbol :reader 'si:*structure-definition-type)
  (documentation "説明文" docstring :reader 'structure-definition-documentation)
  (slot-defs "スロット定義" (list-of text)
    :reader 'structure-definition-slot-descriptions))


;;;; Describe

(defgeneric describe-object (object stream)
  (:documentation
   "OBJECT の説明を STREAM に出力する。"))

(defun %describe-inspectable-bit-value (object inspectable)
  (funcall (inspectable-reader inspectable) object))

(defun %print-inspectable-bit-value (value type &optional (undef-label))
  (cond ((eql value +undefined+)
          (princ undef-label))
        ((listp type)
          (case (car type)
            (list-of
             (let ((type* (second type)))
               (dolist (x value)
                 (%print-inspectable-bit-value x type*)
                 (fresh-line))))
            (plist-of
             (let ((key-type (second type))
                   (value-type (third type)))
               (princ (with-output-to-string (*standard-output*)
                        (do ((rest value (cddr rest)))
                            ((null rest))
                          (%print-inspectable-bit-value (first rest) key-type)
                          (format t "~20,4T= ")
                          (%print-inspectable-bit-value (second rest) value-type)
                          (terpri))))))))
        (:otherwise
          (case type
            (docstring (princ value))
            (boolean   (format t "~:[No~;Yes~]" value))
            (text      (princ value))
            (otherwise (%prin* value))))))

(defun %describe-inspectable-bit (object inspectable out)
  ;; label
  (format out "~&~A:" (inspectable-label inspectable))
  ;; value
  (let ((value-text (with-output-to-string (*standard-output*)
                      (%print-inspectable-bit-value
                       (funcall (inspectable-reader inspectable) object)
                       (inspectable-type inspectable)
                       (inspectable-undefined-label inspectable)))))
    (if (find #\LFD value-text)
      (format out "~%~{    ~A~%~}" (ed:split-string value-text #\LFD))
      (format out "~20,4T~A~%" value-text))))


(defun -%describe-inspectable-bit (object inspectable out)
  (let ((label (inspectable-label inspectable))
        (value (funcall (inspectable-reader inspectable) object))
        (type  (inspectable-type inspectable)))
    (cond ((eq value +undefined+)
            (format out "~&~A:~16,4T~A~%" label (inspectable-undefined-label inspectable)))
          ((listp type)
            (lisp:case (car type)
              (list-of
                (format out "~&~A:~%~{    ~A~%~}" label (mapcar #'%prin*-to-string value)))))
          ((symbolp type)
            (lisp:case type
              (plist
                (format out "~&~A:~%" label)
                (do ((rest value (cddr rest)))
                    ((null rest))
                  (format out "    ~S~20,4T~A~%"
                          (first rest)
                          (%prin*-to-string (second rest)))))
              (docstring
                (format out "~&~A:~%~{    ~A~%~}" label
                        (ed:split-string value #\LFD)))
              (boolean
                (format out "~&~A:~16,4T~:[No~;Yes~]~%" label value))
              (text
                (format out "~&~A:~16,4T~A~%" label value))
              (otherwise
                (format out "~&~A:~16,4T~A~%" label (%prin*-to-string value))))))))

(defmethod describe-object ((object t) stream)
  (let ((out (canonicalize-output-stream stream)))
    (format out "[~S] " (type-of object))
    (%prin* object out)
    (format out "~%------------------------------------------------------------------------~%")
    (dolist (x (inspect-object object))
      (%describe-inspectable-bit object x out))
    (format out "------------------------------------------------------------------------~%")
    ))



(defparameter *describe-depth* 0)

(defparameter *describe-depth-limit* 3)

(defun describe (object &optional (stream *standard-output*))
  (let ((out (canonicalize-output-stream stream)))
    (when (>= *describe-depth* *describe-depth-limit*)
      (%prin* object out))
    (let ((*describe-depth* (1+ *describe-depth*)))
      (describe-object object out)
      (values))))


(provide "ansify/inspect")
;;;; ansify/inspect.l ends here.
