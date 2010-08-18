Common Lisp の xyzzy にないもの詰め合わせ予定地


使い方
======
読み込んでおいて、任意のパッケージ内で cl::install すると使えるようになる。
lisp パッケージとぶつかるものは shadowing-import されて、lisp:xxx じゃなくて cl::xxx が見えるようになる。

    (require "cl")
    (cl::install)


実装済み
========
- typecase
- etypecase
- ctypecase
- print-unreadable-object
- progv
- pprint


依存してるライブラリ
====================
- condition-restart ...そのうち同梱するかも


