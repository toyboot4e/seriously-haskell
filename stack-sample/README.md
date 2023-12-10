# stack-sample

`stack` のサンプルプロジェクトです。  A ~ G 問題までの 7 問を想定したプロジェクト構成となっています。

## 各種ファイル

`stack new stack-sample` 時点で以下のファイルが生成されます。

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
  言語サーバーの動作のために必要です。 `package.yaml` を作成の上で [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (`gen-hie` コマンド) で生成します。

## 操作方法

### スクリプト実行

`Main.hs`  を実行ファイルとして実行します:

```sh
$ ./a/Main.hs
```

もしくは直接 `stack` コマンドの引数とします:

```sh
$ stack ./a/Main.hs
```

> ただし macOS では shebang (`#!/usr/bin/env stack`) を削除した上で `stack` コマンドを使わなければ、サンプルコマンドは実行できないかもしれません。

### コンパイル実行

```hs
$ stack run a-exe
```

### `doctest` の実行

[`hpack`]: https://github.com/sol/hpack

