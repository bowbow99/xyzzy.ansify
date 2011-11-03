これは何
========
ANSI Common Lisp にあって xyzzy にないもの詰め合わせ予定地。


インストール
============

NetInstaller から
-----------------
[カフェイン中毒] からどうぞ。

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l


使い方
======
とりあえず読み込んでおきます。

    (eval-when (:execute :compile-toplevel :load-toplevel)
      (require "ansify"))

ansify で実装している関数やマクロは ansify パッケージから export されています。
一部の関数やマクロが lisp パッケージのシンボルと衝突するので、通常は ansify を
use-package はできません。以下のいずれかの方法で利用してください。

- 必要なシンボルを個別に import あるいは shadowing-import
- `(ansify::install)` で現在のパッケージに ansify から export されている全て
  のシンボルを shadowing-import
- パッケージ名付きで指定する

注意: `(ansify::install)` を使っていると、ansify がバージョンアップして export
されるシンボルが追加された場合に、それらのシンボルも shadowing-import されるよう
になります。
ansify パッケージからは ANSI で決められた名前のシンボルを将来 export する可能性
があるので、ANSI に含まれる名前の関数などを定義しないようにしてください。


注意点、既知の問題など
======================

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.ansify/issues
  [@bowbow99]: http://twitter.com/bowbow99

