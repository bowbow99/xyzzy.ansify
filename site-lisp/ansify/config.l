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

不正と見なされるのは、リストでもシンボルでもない等の型指定子になり得ない
形式のものと、形式としてはありだけど ANSI で禁止されてるもの（`and` を
リストではなくシンボルで、など）が含まれます。

設定できる値:
  nil
  :warn  -- 警告（simple-warning）を投げます。xyzzy 本体の設定次第で
            ダイアログが出たりしますが、処理はそのまま続行されます。
  :error -- エラー（ansify::undefined-type-specifier）を投げます。

デフォルトは :error です。")


(provide "ansify/config")
;;; ansify/config.l ends here.
