# stack-sample

Stack のサンプルプロジェクトです。  A ~ G 問題までの 7 問を想定したプロジェクト構成となっています。

## プロジェクトファイル

`stack new stack-sample` 時点で以下のファイルが生成されます:

- `package.yaml`  
  [`hpack`] が指定するフォーマットです。依存パッケージや実行ファイルの設定をします。

- `stack.yaml`  
  `stack` が指定するフォーマットです。

- `stack-sample.cabal`  
  `stack` により自動生成されます。

- `Setup.hs`  
  使いません。

まだ以下のファイルを追加しました:

- `stack.yaml.lock`  
  `stack` により自動生成されます。

- `hie.yaml`  
  環境に依っては、言語サーバーの動作のために必要です。 `package.yaml` を作成の上で [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (`gen-hie` コマンド) で生成します。

## 操作方法

### REPL の実行

`stack repl` で `Main.hs` を読み込んだ REPL を起動できます:

```sh
$ stack repl a/Main.hs
```

### スクリプト実行

`Main.hs` を Stack スクリプトとして実行します:

> ただし macOS では shebang (`#!/usr/bin/env stack`) を削除した上で `stack` コマンドを使わなければ、サンプルコマンドは実行できないかもしれません。

```sh
$ stack a/Main.hs
$ ./a/Main.hs
```

### コンパイル実行

`stack run` で実行できます:

```hs
$ stack run a-exe
```

### [`doctest`] の実行

ビルド後は Cabal の REPL から [`doctest`] を実行できます:

```hs
$ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault' a-exe
```

Stack project のテストモジュールに [`doctest`] の実行を含めることもできます。

[`hpack`]: https://github.com/sol/hpack
[`doctest`]: https://github.com/doctest/doctest

