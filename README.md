# treep 言語リポジトリ

treep は、表層構文 → EAST 正規化 → マクロ展開 → HM 型推論 → インタプリタ実行という学習しやすいパイプラインを備えたミニ言語です。Scala 3 で実装されており、ファイル拡張子は `.treep` を利用します。

---

## 目次
- [概要](#概要)
- [前提条件](#前提条件)
- [クイックスタート](#クイックスタート)
- [CLI コマンド](#cli-コマンド)
- [言語ハイライト](#言語ハイライト)
- [最初のプログラム](#最初のプログラム)
- [標準ライブラリ早見表](#標準ライブラリ早見表)
- [プロジェクト構成](#プロジェクト構成)
- [開発・テストの進め方](#開発テストの進め方)
- [関連ドキュメント](#関連ドキュメント)

---

## 概要
- **Scala 製の処理系**: ソースは `src/main/scala` にまとまり、CLI からビルド／実行可能。
- **シンプルなパイプライン**: 構文解析 → EAST 正規化 → マクロ展開 → Hindley–Milner 型推論 → インタプリタ実行を一貫して体験できます。
- **教育／実験用途向け**: 拡張メソッド、Row 多相レコード、イテレータなどをコンパクトに確認できます。

## 前提条件
- Java 21 互換 JDK
- [sbt](https://www.scala-sbt.org/) 1.10 以降（動作確認バージョン: 1.10.2）
- macOS / Linux / WSL など POSIX 系シェル環境

---

## クイックスタート
1. 依存解決とビルド
   ```bash
   sbt compile
   ```
2. サンプルを生成（`samples/hello.treep` を作成）
   ```bash
   sbt "run new"
   ```
3. 作成したサンプルを実行
   ```bash
   sbt "run run samples/hello.treep"
   ```
4. 全サンプルを一括で解析／型検査
   ```bash
   sbt "run build"
   ```
5. テストの実行（MUnit）
   ```bash
   sbt test
   ```

---

## CLI コマンド
| コマンド | 説明 |
| --- | --- |
| `sbt "run new"` | `samples/hello.treep` を生成。既存ファイルがある場合は上書き。 |
| `sbt "run build"` | 現在のディレクトリ配下の `.treep` を探索し、構文・マクロ展開・型検査を実施。 |
| `sbt "run run"` | `.treep` を探索し、ゼロ引数 `main` を持つファイルのみ実行。 |
| `sbt "run run path/to/foo.treep"` | 指定したファイルを解析・型検査後に実行。 |
| `sbt "run fmt"` | フォーマッタ (MVP) — 現時点ではノーオペレーション。 |
| `sbt "run test"` | 将来的なテスト統合ポイント（現在は案内メッセージのみ）。 |

---

## 言語ハイライト
- **衛生的マクロ**  
  `for (x in: xs)` のような糖衣構文を EAST 上で `while` + イテレータへ展開し、変数捕捉を避けるよう gensym (`__it$N`) を採用。
- **Hindley–Milner 型推論**  
  Algorithm W をベースに `List[A]`, `Dict[K, V]`, `Iter[T]`, タプル、関数型をサポート。拡張メソッドも型推論に統合。
- **Row 多相レコード**  
  `{ x: T | ρ }` 形式でフィールド不足を許したまま `p.x` を扱えるため、レコード拡張や動的辞書アクセスを自然に表現可能。
- **一貫したメソッド解決**  
  ビルトイン → レコード関数フィールド → トップレベル関数 (`recv` を先頭引数に取る) の順で解決。
- **シンプルな関数記法**  
  ラムダは `(x: Int) -> { ... }`、関数型は `Int -> Int` で右結合。関数値を第一級で扱える。

---

## 最初のプログラム
`samples/hello.treep` を立ち上げ、`println` と戻り値を確認してみましょう。

```treep
def main() returns: Int {
  println("hello from treep!")
  return 0
}
```

実行例:

```bash
sbt "run run samples/hello.treep"
```

```
hello from treep!
[treep] exit 0
```

さらに、辞書とイテレータを組み合わせた例です。

```treep
const m = { "a": 1, "b": 2 }

def sumDict() returns: Int {
  let s = 0
  for (pair in: m) {
    println(pair)
    s = s + snd(pair)
  }
  println("total:")
  println(s)
  return s
}

def main() returns: Int { return sumDict() }
```

---

## 標準ライブラリ早見表

### コレクション
| レシーバ | 主なメソッド |
| --- | --- |
| `List[A]` | `length()`, `head()`, `tail()`, `push(a)`, `append(a)`, `concat(xs)`, `iter()` |
| `Dict[K,V]` | `size()`, `keys()`, `values()`, `entries()`, `hasKey(k)`, `get(k)`, `getOrElse(k, d)`, `put(k, v)`, `remove(k)`, `iter()` |
| `Iter[T]` | `hasNext()`, `next()`, `toList()` |

### タプル・補助
- `fst((A,B))`, `snd((A,B))`
- `print(value)`, `println(value)` — どの値でも表示可能。戻り値は `Unit` (`()` 表記)。

---

## プロジェクト構成
```
.
├─ samples/                  # 言語機能をカバーする .treep サンプル
├─ src/
│  └─ main/scala/com/github/kmizu/treep/
│       ├─ lexer/           # トークナイザ
│       ├─ parser/          # CST と Pratt/LL パーサ
│       ├─ east/            # EAST 定義と正規化
│       ├─ macro/           # マクロ展開
│       ├─ types/           # Type/Scheme/Subst/Unify/HM/Checker
│       ├─ interpreter/     # インタプリタ
│       └─ cli/             # `treep new|build|run|fmt|test`
└─ src/test/scala/          # MUnit テスト
```

---

## 開発・テストの進め方
- コード変更後は必ず `sbt compile` → `sbt test` の順で確認する。
- CLI での簡易チェック:
  - `sbt "run build"` で型検査まで
  - `sbt "run run"` で実行確認
- サンプル追加時は `samples/` に `.treep` を置き、`main` をゼロ引数で用意すると CLI から実行しやすいです。

---

## 関連ドキュメント
- 開発規約・コントリビューションガイド: [`AGENTS.md`](./AGENTS.md)
- 追加のテスト計画やゴールデンテスト案: `modules/tests`, `tests/`（整備予定）

質問や改善アイデアがあれば Issue / PR でお気軽にどうぞ！
