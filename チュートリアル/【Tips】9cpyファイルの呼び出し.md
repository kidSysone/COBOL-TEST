概要
  * [cobの呼び出し](#cobの呼び出し)
    * [cob呼び出し側](#cob呼び出し側)
    * [cob呼び出され側](#cob呼び出され側)
  * [ブック(cpy)の呼び出し](#ブック(cpy)の呼び出し)
    * [cpy呼び出し側](#cpy呼び出し側)
    * [cpy呼び出され側](#cpy呼び出され側)
    * [セッティング](#セッティング)

***
# cobの呼び出し
データ部で共通の領域を作り、引数、戻り値として扱う  

**※※※ 命名、領域、構造を一致しなければならない ※※※**  
**※※※ 呼び出される変数は`レベル01`でなければならない ※※※**

## cob呼び出し側
* データ部：
  * `WORKING-STORAGE SECTION.`などに収納しておｋ
* 手続き部：
  * CALL `呼び出し先.cob` USING `使用する変数`
  * 使用する変数が複数個がある場合、スペースを入れて追加する

```Cobol
PROCEDURE DIVISION.
*> 呼叫 READ-ADDRESS
CALL 'READ-ADDRESS' USING LS-RA.

*> 呼叫 READ-LIST
CALL 'READ-LIST' USING LS-LIST-REC.
```

## cob呼び出され側
* データ部：
  * 必ず`LINKAGE SECTION.`に収納する

```Cobol
DATA DIVISION.
LINKAGE SECTION.
      01 LS-SAF.
            05 NMADR   PIC X(35).
            05 DTLS    PIC X(35) OCCURS 18 TIMES.
            05 LOOP-NO PIC 9.
```
* 手続き部：
  * PROCEDURE DIVISION USING `使用する変数`

```Cobol
PROCEDURE DIVISION USING LS-SAF.
```


*** 
# コピーブック(cpy)の呼び出し
`***.cpy`ファイルで変数を保存して再利用する  
**※※※ あくまでも展開されるテンプレート呼び出しの利用はできるだけ**  
**※※※ 本質的な値や状態の継承は伴わない ※※※**

## cpy呼び出し側
1. 直接の利用  
* 引用元の変数名のままで利用することが可能  

```Cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
      COPY COPYDAT.

PROCEDURE DIVISION.
      MOVE "ABCD" TO STR-DATA01
```

2. REPLACING
* `REPLACING` == '元の変数名' == `BY` == `変更後の変数名` ==.
* 引用元の変数名を別の変数名に変えて利用する
```Cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
      COPY COPYDAT
      REPLACING == STR-DATA01 == BY == REPLACING-DATA01 ==.

PROCEDURE DIVISION.
      MOVE "ABCD" TO REPLACING-DATA01
```

3. すべてを置換、左から
* `LEADING` == '変更したい文字列' == `BY` == `変更後の文字列` ==.
* 左からマッチした内容を置換する
```Cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
      COPY COPY-DATA2    REPLACING
      LEADING  ==STR==  BY ==REPLACING==. 

PROCEDURE DIVISION.
  MOVE "ABCD" TO REPLACING-DATA01.
  MOVE "EFGH" TO REPLACING-DATA02.
  MOVE "IJKL" TO REPLACING-DATA03.
```

4. すべてを置換、右から
* `LEADING` == '変更したい文字列' == `BY` == `変更後の文字列` ==.
* 右からマッチした内容を置換する

```Cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
      COPY COPY-DATA2    REPLACING
      LEADING  ==STR==  BY ==REPLACING==. 
      REPLACING TRAILING ==DATA== BY ==0==.

PROCEDURE DIVISION.
  MOVE "ABCD" TO STR-001.
  MOVE "EFGH" TO STR-002.
  MOVE "IJKL" TO STR-003.
```

## cpy呼び出され側
* `***.cpy`ファイルで保管する
* 領域のみで大丈夫。DIVISIONなどの指定も必要がない
* 8文字以内に抑えるべく

```
01 STR-DATA.
   03 STR-DATA01 PIC X(4).
   03 STR-DATA02 PIC X(4).
   03 STR-DATA03 PIC X(4).
```

## セッティング
* パスがうまく設定できてない時がある  
* `settings.json`で参照すべくフォルダを指定することが可能
* 下記`settings.json`はこのようなフォルダ環境での設定に使う
```
C:\CobolProject\
  ├─ .vscode\settings.json            ←  .json (参照パス設定)
  └─ src\
      ├─ main\       ← メインプログラム (***.cob)
      └─ copy\       ← COPYBOOKS (***.cpy)
```

* パス設定例
```JSON
{
    // コンパイル用の検索パス
    "cobol.copybookdirs": [
        "src/main",
        "src/copy"
    ],

    // エディタ補助用の検索パス
    "coboleditor.copybookdirs": [
        "src/main",
        "src/copy",
        "src/main/copybook",
        "src/main/resources"
    ]
}
```
