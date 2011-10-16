2011-09-30  Ver. 0.02.00
========================
- パッケージ周りを調整
  ANSI 標準のシンボルは "ansify" から export して、それ以外で xyzzy/ansify
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


2011-09-30  Ver. 0.01.01
========================
- バージョン変数 `ansify::+version+` を追加
- バグ2つ修正（thx to @miyamuko）

2011-09-30  Ver. 0.01.00
========================
- 初リリース
- バラバラに作ってた Common Lisp なものを集めた
  - [symbol-macrolet](https://github.com/bowbow99/xyzzy.symbol-macrolet)
  - [condition-restart](https://github.com/bowbow99/xyzzy.condition-restart)
  - [destructuring-bind](https://github.com/bowbow99/xyzzy.destructuring-bind)
  - [typespec+](https://github.com/bowbow99/xyzzy.typespec-plus)
  - [typecase](https://github.com/bowbow99/xyzzy.typecase)
- case, ecase, ccase を実装
- print-unreadable-object を実装
