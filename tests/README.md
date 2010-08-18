xyzzy.common-lisp のテストたち、に関するメモ

lisp-unit を使ってるけど、そのうち lisp-unit を何とかしたいので暫定。

テスト実行
==========
とりあえず *scratch* からやってる。

0. lisp-unit が必要
1. テストファイルを load
2. (use-package :lisp-unit) する
3. (run-all-tests cl.test.${target}) する
4. 結果が表示される


テストの書き方
==============
テストは tests/${target}.lt に書いてる。

1. 専用のパッケージを用意

        (defpackage :cl.test.${target})
        (in-package :cl.test.${target})

2. cl を使えるように準備。

        (require "cl")
        (cl::install)

3. lisp-unit を使えるように準備。
   lisp-unit はテストをパッケージごとにまとめて管理してて、テストファイルを再読み込みしたときに古いテストは一旦全部消してから定義し直す。
    
        (require "lisp-unit")
        (use-package :lisp-unit)
        
        (remove-all-tests)

4. define-test で気が済むまでテストを書く。


