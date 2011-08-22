これは何
========
ANSI Common Lisp にあって xyzzy にないもの詰め合わせ予定地。


インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒] からどうぞ。</del>

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

設定
====
今のところなし。

使い方
======
読み込んでおいて

    (eval-when (:execute :compile-toplevel :load-toplevel)
      (require "ansify"))

任意のパッケージにインストールします。

    (in-package :your-package)
    (eval-when (:execute :compile-toplevel :load-toplevel)
      (ansify::install))

インストールするとそのパッケージ内で ansify で定義してある関数やマクロが見える
ようになります。


注意点、既知の問題など
======================

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.ansify/issues
  [@bowbow99]: http://twitter.com/bowbow99
