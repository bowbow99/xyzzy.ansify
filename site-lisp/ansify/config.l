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

(export '(*signal-on-undefined-typespec*
          *signal-on-invalid-typespec*
          ))

(defvar *signal-on-undefined-typespec* :warn
  "*未定義の型指定子が使われたらエラーや警告を投げるかどうか。

設定できる値:
  nil    -- 何もしません。
  :warn  -- 警告（simple-warning）を投げます。xyzzy 本体の設定次第で
            ダイアログが出たりしますが、処理はそのまま続行されます。
  :error -- エラー（ansify::undefined-type-specifier）を投げます。

デフォルトは :warn です。")

(defvar *signal-on-invalid-typespec* :error
  "*不正な型指定子が使われたらエラーや警告を投げるかどうか。

不正な型指定子と見なされるのは以下の場合:
- `and` や `not` などのリストでなければならない型指定子がシンボルのみで
使用された
- `satisfies` に predicate 関数として lambda 式が指定された
いずれも ANSI では禁止されいるが、xyzzy の `lisp:typep` では通ってしまう。

設定できる値:
  nil    -- 何もしない。
            `and` や `or` の場合は `(and)` や `(or)` を指定されたのと同じ
            事になる。
            `eql` や `not` などの引数が必要な型指定子の場合はエラーになる。
            `satisfies` に lambda 式の場合はそのまま predicate 関数として
            使用する。
  :warn  -- 警告（simple-warning）を投げる。
            xyzzy 本体の設定次第でダイアログが出たりするが、処理はそのま
            ま実行されて最終的には nil の場合と同じ結果になる。
  :error -- エラー（lisp:bad-type-specifier）を投げる

デフォルトは :error です。")


;;;; for typecase.l

(export '(*signal-on-otherwise-in-middle*
          ))

(defvar *signal-on-otherwise-in-middle* :warn
  "*case 式の途中にデフォルト節があった場合にエラーや警告を投げるかどうか。

デフォルト節てのは節のキーが `t` や `otherwise` になっている節。
ANSI を厳密に解釈すると途中に置いてはいけない（置いた場合は

設定できる値:
  nil    -- 途中であってもその節をデフォルト節として扱い、それ以降の節は
            無視される。
            ※xyzzy 標準の `lisp:case` と同じ
  :warn  -- 警告（warning）を投げる。
            xyzzy 本体の設定次第でダイアログが出たりするが、処理はそのま
            ま実行されて最終的には nil の場合と同じ結果になる。
  :error -- エラー（otherwise-in-middle）を投げる。")


(provide "ansify/config")
;;; ansify/config.l ends here.
