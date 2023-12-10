# cabal-sample

## 操作方法

### REPL の実行

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

```hs
$ cabal run a-exe
```

### `doctest` の実行

TODO: 書く

```hs
$ cabal repl
```

