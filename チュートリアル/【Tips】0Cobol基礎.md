概要
- [Cobol自体への紹介](#cobol自体への紹介)
  - [Cobolについて](#cobolについて)
  - [正書法と自由書式](#正書法と自由書式)
- [基本概念](#基本概念)
  - [書き方の基本](#書き方の基本)
  - [ディレクトリでの管理](#ディレクトリでの管理)
  - [ターミナルでの操作](#ターミナルでの操作)
  - [基本DIVISION](#基本division)
    - [IDENTIFICATION DICISION（見出し部、I/O）](#identification-dicision見出し部io)
    - [ENVIRONMENT DIVISON（環境部）](#environment-divison環境部)
    - [DATA DIVISION（データ部）](#data-divisionデータ部)
    - [PROCEDURE DIVISION（手続き部）](#procedure-division手続き部)
      - [パラグラフ（PARAGRAPH）](#パラグラフparagraph)


***
# Cobol自体への紹介
## Cobolについて
* 事務処理（オフコン）
* 事務処理用のプログラムを効率よく開発できる
* COmmmon Business Oriented Language、コボル
* 1950年代からの言語
* 公式サイト：GUN Cobol (OpenCobol)
* 参考サイト：
  * [ドットインストール 様](https://dotinstall.com/lessons/basic_cobol)
    * 無料動画レッスン
  * [株式会社COBOL 様](https://www.cobol.co.jp/cobol-nyuumon/)
* Unixコマンドで操作する 
* COBOL はコンパイラ言語なので、ソースをそのまま実行できない。必ずコンパイルする必要がある
  * -x オプションがないとコンパイルはできても、実行ファイルは作られず .o ファイルだけになる
```
# コンパイル例
cobc -x --free hello.cob # -x = 実行可能ファイル生成。 --free = 自由書式

ls # hello hello.cobというファイルができたのを確認

./hello # 該ファイルを実行する
```

## 正書法と自由書式
* 正書法
  * 固定形式（Fixed-format COBOL）
  * 1959年〜1980年代くらいの標準
  * 従来の正書法は、パンチカードに転写する必要があったため、その名残として6桁の行番号、半角スペース、内文で成り立つ
  * レガシー環境（古いメインフレーム）では推奨される
```Cobol
000000 IDENTIFICATION DIVISION.
000010 PROGRAM-ID.     HELLO.
000020 PROCEDURE       DIVISION.
000030 MAIN            SECTION.
000040 DISPLAY         'HELLO WORD!!'.
000050 STOP            RUN.
```

* 自由書式
  * フリー形式（Free-format COBOL）
  * GnuCOBOLや最近のCOBOLコンパイラで推奨される
  * 現在では自由書式がメインで扱われている。（こちらをメインとして扱っていただく）
```Cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.
PROCEDURE DIVISION.
MAIN SECTION.
DISPLAY 'HELLO WORD!!'.
STOP RUN.
```

***
# 基本概念

## 書き方の基本
* 大文字、小文字も可
  * 現在のCobol環境では小文字も受け入れてきたが、習慣的に大文字が好ましい
* 文末にピリオド（'.'）。命令の終わりと意味する。通常1つのひとまり（ブロック構文）に、1つのピリオドを付ける
* 文字列の扱い。'内文'または"内文"と、シングルクォーテーションまたはダブルクォーテーションで囲む
* コメントアウト
  * 正書法では、7桁目に'*'と記述。列ごとコメントアウト
  * 自由書式では、'*>'と記述。モダン拡張のコメント。列に縛られず、コードの後ろに記述することが可能
  * 特殊：paizaIOは#と記述。COBOL の文法じゃなくて その実行環境（GnuCOBOL on paizaIO）のシェルスクリプト側でのコメント。
    * paizaIO が裏で cobc コンパイル + 実行をラップしてるから、コード冒頭に # 付けても「シェル的に無視されてた」だけ
```Cobol
*> 正書法
* A は１桁の数字
01 A PIC 9.

*> 自由書式
01 A PIC 9.　*> A は１桁の数字
```
* COBOLではインデントが必須ではないが、可読性のため固定幅で揃えるとよい
* 1行は72文字程度を目安に書く（列位置に意味がある場合もある）
* PERFORM で呼ぶパラグラフや SECTION は事前に定義する

## ディレクトリでの管理
```
典型的なディレクトリ構成
C:\CobolProject\
  ├─ src\
  │   ├─ main\       ← メインプログラム (例: EXECUTE.cob)
  │   ├─ copy\       ← COPYBOOKS (共通定義ファイル *.cpy)
  │   └─ modules\    ← サブルーチン (CALLされる *.cob)
  │
  ├─ bin\            ← コンパイル済み exe (実行ファイル)
  ├─ input\          ← 入力ファイル (CobolTest_A.txt など)
  ├─ output\         ← 出力ファイル (CobolTest_B.txt など)
  └─ work\           ← 一時ファイル／ログなど
```

## ターミナルでの操作
基本は
1. 【移動：srcディレクトリ】⇒
2. 【コンパイル：ファイル指定】⇒
3. 【移動：binディレクトリ】⇒
4. 【実行】
上記フォルダ間で移動する
```
PS C:\Cobol> cd C:\Cobol\src
# 同一フォルダに出力したい場合
PS C:\Cobol\src> cobc -x -o test.exe test.cob　
# 出力先を指定したい場合
PS C:\Cobol\src> cobc -x -o ..\bin\EXECUTE.exe EXECUTE.cob　

# 他cobを呼び出す時に、まとめてコンパイル（複数も可能）
PS C:\Cobol\src> cobc -x -o ..\bin\EXECUTE.exe EXECUTE.cob SPLIT-ADDRESS-FIELDS.cob
# 違うフォルダにある場合
PS C:\Cobol\src> cobc -x -o ..\bin\EXECUTE.exe main\EXECUTE.cob modules\SPLIT-ADDRESS-FIELDS.cob　


PS C:\Cobol\src> cd C:\Cobol\bin
PS C:\Cobol\bin> .\test.exe
HELLO,WORLD!
```

## 基本DIVISION
* 4つのDIVISIONがあり、省略するが可能だが、順番を変えてはいけない
  * IDENTIFICATION DICISION（見出し部）
  * ENVIRONMENT DIVISON（環境部、I/O）
  * DATA DIVISION（データ部）
  * PROCEDURE DIVISION（手続き部）

### IDENTIFICATION DICISION（見出し部、I/O）
  * 必ず書かないといけない
  * 内容：プログラムIDを書く
```
IDENTIFICATION DIVISION.   *> プログラムの基本情報を定義する区分
PROGRAM-ID. HELLO.         *> このプログラムの名前は TESTFILE
```

### ENVIRONMENT DIVISON（環境部）
  * コンピューターの実行環境
  * 環境変数などの情報を宣言する構成節`CONFIGURATION SECTION`
  * 入出力の情報`INPUT-OUTPUT SECTION`
  * 省略可
```Cobol
       ENVIRONMENT DIVISION.          *> 環境設定を記述する場所
       INPUT-OUTPUT SECTION.          *> ファイル入出力の設定をする場所
       FILE-CONTROL.                  *> 実際に使うファイルの指定
           SELECT INFILE ASSIGN TO "C:\Cobol\input\CobolTest_A.txt"
              ORGANIZATION IS LINE SEQUENTIAL. *> INFILE を行単位で読み込む
           SELECT OUTFILE ASSIGN TO "C:\Cobol\output\CobolTest_B.txt"
              ORGANIZATION IS LINE SEQUENTIAL. *> OUTFILE を行単位で書き込む
```

### DATA DIVISION（データ部）
  * 省略可
  * ファイル節、入出力のファイル情報を宣言する`FILE SECTION`
    * ファイル記述項（FD）とファイルのレコード定義
  * 作業場所節、変数を宣言する`WORKING-STORAGE SECTION`
    * 初回ロード時だけ初期化
  * 作業場所節、変数を宣言する`LOCAL-STORAGE SECTION`
    * 毎回ロード時に初期化
  * 連絡節、他cobとのリンクを宣言する`LINKAGE SECTION`
  * 通信節、COMMUNICATION SECTION
```
DATA DIVISION.
WORKING-STORAGE SECTION.
```

### PROCEDURE DIVISION（手続き部）
  * 必ず書かないといけない
    * `MAIN SECTION`、メイン処理全体
      * 省略可
    * `INITIALIZATION SECTION`、初期化処理
      * 一般的にはプログラムの最初で呼び出される
    * `PROCESSING SECTION`、主処理
      * データ加工・計算・ファイル操作など
    * `TERMINATION SECTION`、終了処理
      * クローズ処理・ログ出力・STOP RUN
  * STOP RUN.

```Cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE-ADDRESS-PROCESS.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 COUNTER      PIC 9(3) VALUE 0.
01 NAME         PIC X(20) VALUE SPACES.
01 TEMP-STR     PIC X(50) VALUE SPACES.

PROCEDURE DIVISION.

* ================= INITIALIZATION SECTION =================
INITIALIZATION-SECTION.
    MOVE SPACES TO NAME
    MOVE 0 TO COUNTER
    DISPLAY "初始化完成"

* ================= PROCESSING SECTION =================
PROCESSING-SECTION.
    MOVE "John Doe, 12345, London" TO TEMP-STR
    DISPLAY "處理中: " TEMP-STR
    * ここで住所分割などの処理を行う
    ADD 1 TO COUNTER

* ================= TERMINATION SECTION =================
TERMINATION-SECTION.
    DISPLAY "總共處理記錄: " COUNTER
    DISPLAY "程式結束"
    STOP RUN.
```

#### パラグラフ（PARAGRAPH）
* PROCEDURE DIVISIONで使用する
* 処理のまとまり、メソッド的なやつ
* 呼び出すときは `PERFORM パラグラフ名`
* 引数は直接渡せない（COBOL 85 以前の場合）
* 戻り値も基本的にない（状態は WORKING-STORAGE の変数で共有）

* パラグラフの中で、パラグラフえお呼び出すことも可能
* ※ `STOP RUN.`を付けていなかった場合、パラグラフの内容はもう一度執行される
* ※ `ピリオド`、パラグラフの外側のみ付ける。大規模プログラムではこうして避けることが多い（間違って途中でピリオドを付けるとCOBOLはそこで文が終わったと解釈するため）

```Cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 VAR1 PIC 9(2).

PROCEDURE DIVISION.
    PERFORM PARA1       *> COBOL の標準スタイルでは 呼び出し文にピリオドをつけない
    PERFORM PARA2
    STOP RUN.           *> 必ずつけておくように
PERFORM PARA1
PARA1.
    DISPLAY 'Hello'     *> 内側にピリオドを付けないことによって、ひとまりの開始と終わりがわかりやすくなる
    .

PARA2.
    ADD 1 TO VAR1.
    DISPLAY VAR1
    .
```
