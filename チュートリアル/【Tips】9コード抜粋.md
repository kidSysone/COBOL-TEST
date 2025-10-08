概要
  * [コード抜粋](#コード抜粋)
    ** [特殊文字の判断](#特殊文字の判断)
***
# コード抜粋
よく使われる構文を覚えていきたい所存

## 特殊文字の判断
以下より、`WORK-STR`に特殊文字が含まれているかどうかを判断する
```Cobol
*> 特殊文字判断用の変数
01 WS-CH                  PIC X.
01 WS-CODE                PIC 9(5).
01 ALLOWED-CH             PIC X(20) VALUE "/-?:().,+'".
01 ERROR-FLAG             PIC X.


MOVE "****" TO WORK-STR.
MOVE "N" TO ERROR-FLAG.
PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
  LENGTH OF FUNCTION TRIM(WORK-STR)

  MOVE WORK-STR(IDX:1) TO WS-CH
  COMPUTE WS-CODE = FUNCTION ORD(WS-CH)
  IF WS-CODE < 32 OR WS-CODE > 126
    MOVE "Y" TO ERROR-FLAG
  ELSE
    INSPECT ALLOWED-CH TALLYING WS-CODE FOR ALL WS-CH
    IF NOT((WS-CH >= "0" AND WS-CH <= "9") OR 
      (WS-CH >= "A" AND WS-CH <= "Z") OR
      (WS-CH >= "a" AND WS-CH <= "z") OR
      WS-CH = SPACE OR
      WS-CODE > 0)
        MOVE "Y" TO ERROR-FLAG
    END-IF
  END-IF
END-PERFORM.
IF ERROR-FLAG = "Y"
  *> 特殊文字が含まれている時の処理
  EXIT PERFORM
END-IF.

```