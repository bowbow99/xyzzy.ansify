Ver. 0.04.xx （予定）
=====================

- `ansify.ext:function-info`
  - シンボルのプロパティやハッシュテーブルみたいに、関数オブジェクトに
    データを保存しておくもの。
  - `(setf (function-info #'FUNC 'KEY) DATA)` で保存
  - `(function-info #'FUNC 'KEY)` で取り出し

- `ansify.ext:fwrapper`
  - 関数が呼び出された時に処理を乗っ取るなにか。
  - `(ansify.ext:define-fwrapper NAME ...)` で fwrapper を定義しておいて、
    `(ansify.ext:fwrap #'FUNC 'NAME)` で fwrap する。
  - fwrap すると、fwrap された関数が呼び出された時に fwrapper の本体が
    実行される。
  - fwrapper の本体の中から `(ansify.ext:call-next-function ...)` すると
    本来の処理（fwrap された方）を呼び出せる。

- `trace`, `untrace` を追加

- GFM (Generic Function Modoki)
  - 総称関数の半端な実装
  - 総称関数側: 引数のクラスによる dispatch するだけ
  - クラス側: built-in-class のようなものだけ実装（structure-class は
    `si:*structure-subtypep` をそのまま使ってる）
  - キャッシュとかしてない
  - FIXME: ここに追加したシンボルを書く

- バグ修正など
  - `(defun (setf READER) ..)` で定義した関数のブロック名がおかしかった
    のを、ちゃんと `READER` になるように修正
- 細々したオペレータ
  - `function-lambda-expression`


Ver. 0.03.xx
============

2012-03-29  Ver. 0.03.03
------------------------
- リファレンスに typo や古い記述があったのを修正; thx to @youz
- リファレンスが間違って SJIS になってたのを修正; thx to @youz

2012-02-16  Ver. 0.03.02
------------------------
- リファレンスを生成できないのがバレたので xy-reference から見える所へ
  引っ越し; thx to @miyamuko

2012-01-09  Ver. 0.03.01
------------------------
- cmu_loop 使うのをやめた
- パッケージ "compiler" のパッケージ定義だけ同梱  
  compile.l を丸ごと読み込まずに `compiler::optimize-form` を使えるように
- バグ修正など
  - ansify をダンプイメージに含めると `optimize-type-check` を利用したマクロ
    （`typecase` など）が壊れてた; thx to @miyamuko
  - `ansify:defun` のインデント情報が無かった
  - `ansify:symbol-macrolet` で `multiple-value-setq` の多値式に symbol macro
    を置くと展開されてなかった; thx to @miyamuko

2011-11-04  Ver. 0.03.00
------------------------
- 一部の型指定子をグローバルに修正
  - `eql` -- 不要な deftype による定義を削除（実は前のバージョンでやってた）
  - `real`, `cons` -- リストでの指定にできなかったのを修正
  - この修正は lisp パッケージの方を直接修正してるのでグローバルに影響するけど
  正しく書かれたプログラムでは問題は起きないはず。
- setf function サポート
  `(setf READER)` という形式の関数名を使えるようにした。
  - `defun`
  - `fdefinition`
  - `fboundp`
  - `fmakunbound`
- 細々したオペレータを追加
  - `sleep`

Ver. 0.02.xx
============

2011-10-16  Ver. 0.02.00
------------------------
- パッケージ周りを調整
  - ANSI 標準のシンボルは "ansify" から export して、それ以外で xyzzy/ansify
  特有の機能などは "ansify.ext" から export するように
  ちゃんと export するようにしたので `ansify:etypecase` とか書けるようになった
- 細々したオペレータを追加
  - `type-of`（structure に対して `lisp:structure` ではなく構造体名を返す）
  - `constantly`
  - `the`
  - `nth-value`
  - `upgraded-complex-part-type`
  - 単に名前が変わったもの
    - `special-operator-p`
    - `get-setf-expansion`
    - `define-setf-expander`
- バグ修正（主に typespec 周り）

Ver. 0.01.xx
============

2011-10-05  Ver. 0.01.01
------------------------
- バージョン変数 `ansify::+version+` を追加
- バグ2つ修正（thx to @miyamuko）

2011-09-30  Ver. 0.01.00
------------------------
- 初リリース
- バラバラに作ってた Common Lisp なものを集めた
  - [symbol-macrolet](https://github.com/bowbow99/xyzzy.symbol-macrolet)
  - [condition-restart](https://github.com/bowbow99/xyzzy.condition-restart)
  - [destructuring-bind](https://github.com/bowbow99/xyzzy.destructuring-bind)
  - [typespec+](https://github.com/bowbow99/xyzzy.typespec-plus)
  - [typecase](https://github.com/bowbow99/xyzzy.typecase)
- case, ecase, ccase を実装
- print-unreadable-object を実装
