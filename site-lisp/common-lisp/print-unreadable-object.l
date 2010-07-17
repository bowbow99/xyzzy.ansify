;;; -*- mode: lisp; package: common-lisp -*-
;;;
;;; common-lisp/print-unreadable-object.l
;;;
;;; [CLHS: Macro PRINT-UNREADABLE-OBJECT]
;;; - http://www.lispworks.com/documentation/lw50/CLHS/Body/m_pr_unr.htm#print-unreadable-object
;;;

(require "common-lisp/package")

(in-package :common-lisp)

(export '(print-unreadable-object))


(defun decode-stream-designator (designator)
  (cond ((streamp designator) designator)
        ((eq designator t) *terminal-io*)
        ((eq designator nil) *standard-output*)
        (t (error 'type-error :datum designator
                  :expected-type '(or stream (member t nil))))))

(defmacro print-unreadable-object ((object stream &key type identity)
                                   &body body)
  (let ((obj (gensym))
        (out (gensym))
        (type? (gensym))
        (identity? (gensym))
        )
    `(let ((,obj ,object)
           (,out (decode-stream-designator ,stream))
           (,type? ,type)
           (,identity? ,identity)
           )
       (format ,out "#<")
       (when ,type?
         (format ,out "~S" (type-of ,obj))
         ,(when (or body identity) `(write-char #\SPC ,out)))
       ,@body
       (when ,identity?
         ,(when body `(write-char #\SPC ,out))
         (format ,out "~D" (si:address-of ,obj)))
       (format ,out ">"))))

(provide "common-lisp/print-unreadable-object")

;;; common-lisp/print-unreadable-object.l ends here
