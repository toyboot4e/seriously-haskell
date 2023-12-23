# cabal-sample

Cabal のサンプルプロジェクトです。  A ~ G 問題までの 7 問を想定したプロジェクト構成となっています。

## プロジェクトファイル

`cabal init` 時点で以下のファイルが生成されます:

- `cabal-sample.cabal`

`cabal.project` を追加しても良いかもしれません。

## 操作方法

### REPL の実行

`cabal repl` で `Main.hs` を読み込んだ REPL を起動できます:

```sh
$ stack repl a/Main.hs
```

### スクリプト実行: 不可

Cabal プロジェクト中のファイルを `cabal run` で選択すると、スクリプト実行できません。親切なエラーが裏目に出ている気がします。

```
$ cabal run app/Main.hs
Error: cabal: The run command can only run an executable as a whole, not files
or modules within them, but the target 'app/Main.hs' refers to the file
app/Main.hs in the executable cabal-sample.
```

### コンパイル実行

`cabal run` で実行できます:

```hs
$ cabal run a-exe
```

### [`doctest`] の実行

Cabal の REPL から [`doctest`] を実行できます:

```hs
$ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault' a-exe
```

[`doctest`]: https://github.com/doctest/doctest

