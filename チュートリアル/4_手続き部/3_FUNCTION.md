概要
***


# FUNCTION関数一覧
## 文字列関連
### 長さの計算
`LENGTH OF TMP-STR` または  
`FUNCTION LENGTH()`

### 文字列を大文字にする
`FUNCTION UPPER-CASE(`文字列`)`

### 文字列を小文字にする
`FUNCTION LOWER-CASE(`文字列`)`

### 文字列の反転
`FUNCTION REVERSE(`文字列`)`  

* 前半未使用の余りスペース空間のみを計算したい場合、利用する

```Cobol
INSPECT FUNCTION REVERSE(TP-NAME) TALLYING TP-LEN FOR LEADING SPACES
```

### 余白空間を刈り込む
`FUNCTION TRIM(`文字列`)`  

### 文字列の連結
`FUNCTION CONCAT(`文字列 文字列...`)`  

* ２個以上の文字列を連結させることができる
* STRINGと同じように、TRINしとかないと空白が混ざってしまう

### 文字コードの取得
`FUNCTION ORD(`文字列`)`  
* 文字コード（ASCII 値や EBCDIC 値）を返す
* 文字列の先頭1文字のコードを返す

| 文字 | ORD() の値 |
| -- | -------- |
| A  | 65       |
| a  | 98       |
| 0  | 49       |
| 空白 | 33     |

### 文字コードから文字を取得
`FUNCTION CHAR(`数字`)`

## 数字関連

### 絶対値
`FUNCTION ABS(`数字`)`  
* PIC S9. みたいなデータ領域

### 最大値
`FUNCTION MAX(`数字 数字...`)`  
* ２個以上の数字で比較することができる

### 最小値
`FUNCTION MIN(`数字 数字...`)`
* ２個以上の数字で比較することができる

### 割り算の余りの取得
1. `FUNCTION MOD(`被除数 除数`)`
    * 余りの符号は**除数**に揃う。数学的なmod
2. `FUNCTION REM(`被除数 除数`)`
    * 余りの符号は**被除数**に揃う。算術的なremainder

```Cobol
DISPLAY FUNCTION MOD(  7,  3).   *> 1   (7 ÷ 3 = 2 余り 1)
DISPLAY FUNCTION REM(  7,  3).   *> 1

DISPLAY FUNCTION MOD( -7,  3).   *> 2   (商は -3、余り +2：除数3に符号合わせ)
DISPLAY FUNCTION REM( -7,  3).   *> -1  (商は -2、余り -1：被除数-7に符号合わせ)

DISPLAY FUNCTION MOD(  7, -3).   *> -2  (除数 -3 に符号合わせ)
DISPLAY FUNCTION REM(  7, -3).   *>  1  (被除数 7 に符号合わせ)

DISPLAY FUNCTION MOD( -7, -3).   *> -1  (除数 -3 に符号合わせ)
DISPLAY FUNCTION REM( -7, -3).   *> -1  (被除数 -7 に符号合わせ)
```

### 小数切り捨て
`FUNCTION INTEGER(`数字`)`

### 小数部の内容を取得
`FUNCTION FRACTION-PART(`数字`)`

### 符号付きの判断
`FUNCTION SIGN(`数字`)`

* プラス   -> +1
* マイナス ->  1
* " "（指定なし） ->  1

## 数学関連
### 平方根
`FUNCTION SQRT(`数字`)`
* 数字がマイナスの場合、0との結果になる

### e^x
`FUNCTION EXP(`数字`)`

### 自然対数
`FUNCTION LOG(`数字`)`

### 常用対数
`FUNCTION LOG10(`数字`)`

### 三角関数
`FUNCTION SIN(`数字`)`
`FUNCTION COS(`数字`)`
`FUNCTION TAN(`数字`)`

### 逆三角関数
`FUNCTION ACOS(`数字`)`
`FUNCTION ASIN(`数字`)`
`FUNCTION ATAN(`数字`)`

## 日付・時間系
### YYYYMMDDHHMMSShhmm 形式の文字列を返す
`FUNCTION CURRENT-DATE`  
* 例：2025100109485677+0000

### プログラムがコンパイルされた日時
`FUNCTION WHEN-COMPILED`  
* 例：01/10/2509.48.56
```Cobol
       WORKING-STORAGE SECTION.
       01 WS-DATE-TIME.
          05 WS-DATE     PIC 9(8).   *> YYYYMMDD
          05 WS-TIME     PIC 9(8).   *> hhmmsscc (cc=1/100秒)
          05 WS-OFFSET   PIC S9(4).  *> タイムゾーンオフセット

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE-TIME
           DISPLAY "DATE = " WS-DATE
           DISPLAY "TIME = " WS-TIME
           DISPLAY "OFFSET = " WS-OFFSET
           STOP RUN.
```

## その他
### 乱数
`FUNCTION RANDOM`  
* 0.0 ～ 1.0 の乱数を生成
```Cobol
01 WS-RAND-FLOAT USAGE COMP-1.
01 WS-RAND-INT   PIC 9(5).

COMPUTE WS-RAND-FLOAT = FUNCTION RANDOM

*> 0～99 の整数にスケール
COMPUTE WS-RAND-INT = FUNCTION INTEGER(WS-RAND-FLOAT * 100)
DISPLAY "RANDOM INT   = " WS-RAND-INT
```



# 未開拓ゾーン

FUNCTION CURRENT-DATE → YYYYMMDDHHMMSShhmm 形式の文字列を返す
FUNCTION DATE-OF-INTEGER(n) → 「通算日数」から日付へ変換
FUNCTION INTEGER-OF-DATE(YYYYMMDD) → 日付を「通算日数」に変換
FUNCTION DAY-OF-INTEGER(n) → 日付に変換して YYYYMMDD を返す
FUNCTION INTEGER-OF-DAY(YYYYDDD) → 日付を通算日に変換