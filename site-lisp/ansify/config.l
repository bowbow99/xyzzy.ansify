;;; -*- mode: lisp; package: ansify -*-
;;;
;;; ansify/config.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "ansify".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify/base"))

(in-package :ansify.ext)


;;;; for typespec+

(export '(*warn-on-undefined-typespec*
          *warn-on-invalid-typespec*
          ))

(defvar *warn-on-undefined-typespec* nil
  "*未定義の型指定子が使われたら警告するかどうか。

`typep`, `typecase`, `check-type` など、オブジェクトが型指定子の型かど
うか調べるオペレータの動作に影響する。")

(defvar *warn-on-invalid-typespec* nil
  "*不正な型指定子が使われたら警告するかどうか。

不正な型指定子と見なされるのは以下の場合:
- `and` や `not` などのリストでなければならない型指定子をシンボルのみで
  使用した
- `satisfies` の predicate 関数をシンボル以外で指定した

いずれも ANSI では禁止されているが、xyzzy 標準の `lisp:typep` ではエラー
などにはならない。
リストでなければならない型指定子は、引数なしのリスト形式だったものとして
扱われる。（引数が必要な場合はエラーになる。）
`satisfies` では lambda 式や `#'some-function` のような形式でもその関数
を使う。関数でなかった場合はエラーになる。
")


;;;; for typecase.l

(export '(*warn-on-otherwise-in-middle*
          ))

(defvar *warn-on-otherwise-in-middle* nil
  "*case 式の途中に otherwise 節があった場合に警告するかどうか。

otherwise 節てのは節のキーが `t` や `otherwise` になっている節。ANSI を
厳密に解釈するとこれを途中に置いてはいけない。

case 式の途中であっても otherwise 節があればそこで分岐は終了し、以降の節
は無視される。")


(provide "ansify/config")
;;; ansify/config.l ends here.
