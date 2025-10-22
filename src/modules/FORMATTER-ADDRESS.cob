       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATTER-ADDRESS.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *******************************************************
      *> 資料部、LOCAL-STORAGE SECTION
      *******************************************************
       LOCAL-STORAGE SECTION.
       01 TEMP-ARRAY.
           05 TEMP-PART        PIC X(500) OCCURS 30 TIMES.
           05 TEMP-PART-CHECK  PIC 99     OCCURS 30 TIMES. *> 該IDX歸類位置
           05 MATCH-NEW        PIC 999.
           05 CNT-NUM          PIC 9(3)   OCCURS 30 TIMES. *> 數字字數
           05 CNT-U            PIC X      OCCURS 30 TIMES. *> 大寫字字數

           05 TEMP-A           PIC X(500).
           05 TEMP-B           PIC X(500).
           05 TEMP-COL         PIC X(500).
           05 TEMP-COL-2       PIC X(500).
           05 TEMP-UPPER-COL   PIC X(500).
           05 TEMP-LEN         PIC 999.
           05 TEMP-FLAG        PIC XX VALUE "Y".
           05 DTLS-FLAG        PIC 99.
           05 ZIP-FLAG         PIC X. *> 英國等地區之郵遞區號

           05 CNT-F            PIC 9.
           05 CNT-FL           PIC 9.

           05 PRE-COL          PIC X(100).
           05 PRE-LEN          PIC 999.
           05 PRE-FLAG         PIC XX.

           05 NEXT-COL         PIC X(100).
           05 NEXT-UPPER-COL   PIC X(100).
           05 NEXT-LEN         PIC 999.
           05 NEXT-FLAG        PIC XX.

           05 CHECK-COL        PIC X(100). *> LS-LIST-COL
           05 CHECK-LEN        PIC 999.

           05 DIR-LEN PIC 99 VALUE 21.
           05 DIR-COL  PIC X(8).
           05 DIR-FLAG PIC X(1).

           05 STATE-FLAG       PIC X.

           05 OTHER-FLAG         PIC X.           
           05 OTHER-STREET       PIC 99. *> 5
           05 OTHER-DISTRICT     PIC 99. *> 4
           05 OTHER-CITY         PIC 99. *> 3
           05 OTHER-PROVINCE     PIC 99. *> 16
           05 OTHER-PRE          PIC 99. *> 前次記錄位置

       01 ERROR-ARRAY.
           05 ERROR-TEMP PIC X(40).
           05 COMMA-FLAG PIC XX.
           05 CHARACTERS-FLAG PIC X(40).

       01 IDX PIC 99999.
       01 JDX PIC 999.
       01 KDX PIC 99.
       01 IDX-PLUS PIC 99.
       01 FOUND-JDX PIC 999.
       01 END-JDX PIC 999.
       01 PROCESSING-DATA PIC X(500). *> 處理中資料

       *> 特殊字判斷(ERROR)
       01 WS-SPECIAL-CHAR.
           05 WS-CH                  PIC X.
           05 WS-CODE                PIC 9(5).
           05 ALLOWED-CH             PIC X(20) VALUE "/-?:().,+'".
           05 ERROR-FLAG             PIC X.


      *******************************************************
      *> 資料部、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-FORMATTER.
           05 BEFORE-DATA  PIC X(500). *> 格式化讀取資料
           05 AFTER-DATA   PIC X(500). *> 格式化回傳資料
           05 ORIGIN-DATA  PIC X(500). *> 原文
           05 DTLS-LF      PIC X(500) OCCURS 25 TIMES. *> 地址欄位


       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-STATE-NAME-COL    PIC X(45) OCCURS 200 TIMES.
           05  LS-STATE-CODE-COL    PIC X(10) OCCURS 200 TIMES.
           05  DIR-NAMES OCCURS 21 TIMES PIC X(8). *> 全方向

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-LIST-REC LS-FORMATTER.

      *******************************************************
      *> INITIALIZATION SECTION 初始化
      *******************************************************
       INITIALIZATION SECTION.
           *> TEMP-PART
           *> TEMP-PART-CHECK CNT-NUM 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              MOVE SPACES TO TEMP-PART(IDX)
              MOVE 0      TO TEMP-PART-CHECK(IDX) CNT-NUM(IDX)
                             OTHER-STREET OTHER-CITY OTHER-DISTRICT
                             OTHER-PROVINCE
           END-PERFORM.

           *> MATCH-NEW 初期化
           MOVE 0 TO MATCH-NEW.

           *> AFTER-DATA 初期化
           MOVE SPACES TO AFTER-DATA.

           *> BEFORE-DATA 初期化
           MOVE FUNCTION TRIM(BEFORE-DATA) TO BEFORE-DATA

           *> DTLS-LF 初期化
            PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                MOVE SPACES TO DTLS-LF(IDX) 
            END-PERFORM.

           *> "," -> ", "
           MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TEMP-LEN
               IF BEFORE-DATA(IDX:1) = "," AND IDX = 1
                 SUBTRACT 1 FROM TEMP-LEN IDX
                 MOVE FUNCTION TRIM(BEFORE-DATA(2:TEMP-LEN))
                   TO BEFORE-DATA
                 MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN
               END-IF

               IF BEFORE-DATA(IDX:1) = "," AND IDX = TEMP-LEN
                 SUBTRACT 2 FROM IDX
                 MOVE FUNCTION TRIM(BEFORE-DATA(1:TEMP-LEN - 1))
                   TO BEFORE-DATA
                 MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN
               END-IF

               IF BEFORE-DATA(IDX:2) = ",,"
                 MOVE SPACES TO TEMP-COL

                 STRING
                   BEFORE-DATA(1:IDX) DELIMITED BY SIZE
                   BEFORE-DATA(IDX + 2:TEMP-LEN - IDX - 1)
                     DELIMITED BY SIZE
                   INTO TEMP-COL
                 END-STRING
                 SUBTRACT 1 FROM IDX
                 MOVE TEMP-COL TO BEFORE-DATA
                 MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN

               END-IF


               IF BEFORE-DATA(IDX:1) = ","
                  AND BEFORE-DATA(IDX + 1:1 - NEXT-LEN) NOT = " "
                     STRING
                       BEFORE-DATA(1:IDX) DELIMITED BY SIZE
                       " "  DELIMITED BY SIZE
                       BEFORE-DATA(IDX + 1 - NEXT-LEN :TEMP-LEN - IDX)
                            DELIMITED BY SIZE
                       INTO TEMP-COL
                     END-STRING
                    MOVE TEMP-COL TO BEFORE-DATA
                  MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN
               END-IF
           END-PERFORM.


           *> 迴避複數空格的狀況、縮寫地址插入".""
           UNSTRING 
             BEFORE-DATA DELIMITED BY ALL " "
             INTO TEMP-PART (1)
               TEMP-PART (2)
               TEMP-PART (3)
               TEMP-PART (4)
               TEMP-PART (5)
               TEMP-PART (6)
               TEMP-PART (7)
               TEMP-PART (8)
               TEMP-PART (9)
               TEMP-PART (10)
               TEMP-PART (11)
               TEMP-PART (12)
               TEMP-PART (13)
               TEMP-PART (14)
               TEMP-PART (15)
               TEMP-PART (16)
               TEMP-PART (17)
               TEMP-PART (18)
               TEMP-PART (19)
               TEMP-PART (20)
               TEMP-PART (21)
               TEMP-PART (22)
               TEMP-PART (23)
               TEMP-PART (24)
               TEMP-PART (25)
               TEMP-PART (26)
               TEMP-PART (27)
               TEMP-PART (28)
               TEMP-PART (29)
               TEMP-PART (30)

           MOVE SPACES TO PROCESSING-DATA.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF TEMP-PART(IDX) NOT = SPACES
                MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO TEMP-COL
                MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
                IF TEMP-COL(TEMP-LEN:1) = ","
                   MOVE TEMP-COL(1:TEMP-LEN - 1) TO TEMP-COL
                END-IF

              MOVE "N" TO TEMP-FLAG
              MOVE LENGTH OF FUNCTION TRIM(TEMP-PART(IDX)) TO TEMP-LEN
              IF TEMP-PART(IDX)(TEMP-LEN:1) = ","
                MOVE 1 TO NEXT-LEN
              ELSE
                MOVE 0 TO NEXT-LEN
              END-IF

              *> 2文字
              MOVE TEMP-PART(IDX)(TEMP-LEN - 1 - NEXT-LEN:2)TO TEMP-COL
              IF TEMP-COL = "St"
                 OR TEMP-COL = "Rd"
                 OR TEMP-COL = "Dr"
                 OR TEMP-COL = "Rm"
                 MOVE "Y" TO TEMP-FLAG
              END-IF

              *> 3文字
              MOVE TEMP-PART(IDX)(TEMP-LEN - 2 - NEXT-LEN:3)TO TEMP-COL
              IF TEMP-COL = "Ave"
                 OR TEMP-COL = "Riv"
                 MOVE "Y" TO TEMP-FLAG
              END-IF

              *> 4文字
              MOVE TEMP-PART(IDX)(TEMP-LEN - 3 - NEXT-LEN:4)TO TEMP-COL
              IF TEMP-COL = "Blvd"
                 MOVE "Y" TO TEMP-FLAG
              END-IF

              *> 補上.
              IF TEMP-FLAG = "Y"
                 STRING 
                     TEMP-PART(IDX)(1:TEMP-LEN - NEXT-LEN)
                     "." DELIMITED BY SIZE
                     INTO TEMP-PART(IDX)
                 END-STRING
              END-IF
               STRING
                 FUNCTION TRIM(PROCESSING-DATA) DELIMITED BY SIZE
                 " " DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-PART(IDX)) DELIMITED BY SIZE
                 INTO PROCESSING-DATA
               END-STRING
             END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(PROCESSING-DATA) TO PROCESSING-DATA.


           *> TEMP-PART 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              MOVE SPACES TO TEMP-PART(IDX) 
           END-PERFORM.

      *******************************************************
      *> MAIN SECTION 主要程序
      *******************************************************
       MAIN SECTION.
           UNSTRING 
             BEFORE-DATA DELIMITED BY ALL " "
             INTO TEMP-PART (1)
               TEMP-PART (2)
               TEMP-PART (3)
               TEMP-PART (4)
               TEMP-PART (5)
               TEMP-PART (6)
               TEMP-PART (7)
               TEMP-PART (8)
               TEMP-PART (9)
               TEMP-PART (10)
               TEMP-PART (11)
               TEMP-PART (12)
               TEMP-PART (13)
               TEMP-PART (14)
               TEMP-PART (15)
               TEMP-PART (16)
               TEMP-PART (17)
               TEMP-PART (18)
               TEMP-PART (19)
               TEMP-PART (20)
               TEMP-PART (21)
               TEMP-PART (22)
               TEMP-PART (23)
               TEMP-PART (24)
               TEMP-PART (25)
               TEMP-PART (26)
               TEMP-PART (27)
               TEMP-PART (28)
               TEMP-PART (29)
               TEMP-PART (30)

           *> ======================= 特殊字 抽出 =======================
           *> 抽出 COUNTRY
           MOVE 0 TO PRE-LEN.
           MOVE LENGTH OF FUNCTION TRIM(PROCESSING-DATA) TO NEXT-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO TEMP-COL
             MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
             ADD 1 TO PRE-LEN
             IF (TEMP-PART(IDX + 1) = SPACES OR *> 為最後一串字/國家 + 郵遞區號
                (TEMP-PART(IDX + 1) IS NUMERIC AND 
                 TEMP-PART(IDX + 2) = SPACES)) AND

                (TEMP-LEN = 2 AND
                 TEMP-COL(1:2) IS ALPHABETIC-UPPER)

                   MOVE TEMP-COL TO DTLS-LF(2)
                   IF TEMP-PART(IDX + 1) = SPACES
                     MOVE PROCESSING-DATA(1:PRE-LEN - 3) TO TEMP-COL
                   ELSE
                     STRING
                       PROCESSING-DATA(1:PRE-LEN - 3) DELIMITED BY SIZE
                       PROCESSING-DATA(PRE-LEN + TEMP-LEN + 1:
                         NEXT-LEN - PRE-LEN - TEMP-LEN) 
                         DELIMITED BY SIZE
                       INTO TEMP-COL
                     END-STRING
                   END-IF

                   MOVE TEMP-COL TO PROCESSING-DATA
                   EXIT PERFORM
             END-IF
             ADD TEMP-LEN TO PRE-LEN
           END-PERFORM.

           *> 抽出 STATE
           *> 簡寫STATE抽出作業
           MOVE FUNCTION TRIM(PROCESSING-DATA) TO TEMP-COL.
           INSPECT TEMP-COL REPLACING ALL "&" BY "A".
           MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN.
           MOVE "N" TO STATE-FLAG.
           PERFORM VARYING IDX FROM TEMP-LEN BY -1 UNTIL IDX = 0

              IF ((TEMP-COL(IDX:1) = SPACE OR 
                   TEMP-COL(IDX:1) = ",") AND
                  (TEMP-COL(IDX + 4:1) = SPACE OR 
                   TEMP-COL(IDX + 4:1) = ","   OR
                   IDX + 3 = TEMP-LEN) AND
                 IDX + 3 <= TEMP-LEN AND
                 TEMP-COL(IDX + 1:3) IS ALPHABETIC-UPPER)
                 MOVE PROCESSING-DATA(IDX + 1:3) TO DTLS-LF(17)
                 MOVE SPACES TO PROCESSING-DATA
                 STRING
                   TEMP-COL(1:IDX) DELIMITED BY SIZE
                   TEMP-COL(IDX + 5:TEMP-LEN - 4 - IDX)
                     DELIMITED BY SIZE
                   INTO PROCESSING-DATA
                 END-STRING
                 MOVE "Y" TO STATE-FLAG
                 EXIT PERFORM
              END-IF

              IF ((TEMP-COL(IDX:1) = SPACE OR
                   TEMP-COL(IDX:1) = ",") AND
                  (TEMP-COL(IDX + 3:1) = SPACE OR
                   TEMP-COL(IDX + 3:1) = ","   OR
                   IDX + 2 = TEMP-LEN) AND
                 IDX + 2 <= TEMP-LEN AND
                 TEMP-COL(IDX + 1: 2) IS ALPHABETIC-UPPER)
                 MOVE PROCESSING-DATA(IDX + 1:2) TO DTLS-LF(17)
                 MOVE SPACES TO PROCESSING-DATA
                 STRING
                   TEMP-COL(1:IDX) DELIMITED BY SIZE
                   TEMP-COL(IDX + 4:TEMP-LEN - 3 - IDX)
                     DELIMITED BY SIZE
                   INTO PROCESSING-DATA
                 END-STRING
                 MOVE "Y" TO STATE-FLAG
                 EXIT PERFORM
              END-IF
           END-PERFORM.

           *> 全名STATE抽出作業
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 500
              IF LS-STATE-NAME-COL(IDX) = SPACES OR STATE-FLAG = "Y"
                EXIT PERFORM
              END-IF

              MOVE LS-STATE-NAME-COL(IDX) TO NEXT-COL
              MOVE LENGTH OF FUNCTION TRIM(NEXT-COL) TO NEXT-LEN
              MOVE 0 TO FOUND-JDX
              PERFORM VARYING JDX FROM 1 BY 1
                UNTIL JDX > TEMP-LEN - NEXT-LEN + 1
       
                  IF TEMP-COL(JDX:NEXT-LEN) = NEXT-COL(1:NEXT-LEN)
                   *> 確認該單字前後皆為空白後才能進入
                   IF (
             JDX > 1 AND JDX < TEMP-LEN AND 
             FUNCTION TRIM(TEMP-COL(JDX - 1:NEXT-LEN + 2)) = NEXT-COL OR
             FUNCTION TRIM(TEMP-COL(JDX - 1:NEXT-LEN + 2)) = TEMP-COL-2
             )
                      MOVE JDX TO FOUND-JDX
                      EXIT PERFORM
                     END-IF
                  END-IF
              END-PERFORM
       
              *> 若找到相符內容
              IF FOUND-JDX > 0
                 MOVE SPACES TO PROCESSING-DATA
                 STRING
                   TEMP-COL(1:FOUND-JDX - 1) DELIMITED BY SIZE
                   TEMP-COL(FOUND-JDX + NEXT-LEN + 1:
                            TEMP-LEN - FOUND-JDX)
                            DELIMITED BY SIZE
                   INTO PROCESSING-DATA
                 END-STRING
                 MOVE LS-STATE-CODE-COL(IDX) TO DTLS-LF(17)
                 EXIT PERFORM

              END-IF
           END-PERFORM.

           *> ===================== 特殊字 抽出完成 =====================
      *    DISPLAY "BEFORE: "FUNCTION TRIM(BEFORE-DATA).
      *    DISPLAY "PROCES: "FUNCTION TRIM(PROCESSING-DATA).


           *> 資料分類
           UNSTRING PROCESSING-DATA
             DELIMITED BY SPACE
             INTO TEMP-PART(1)
               TEMP-PART(2)
               TEMP-PART(3)
               TEMP-PART(4)
               TEMP-PART(5)
               TEMP-PART(6)
               TEMP-PART(7)
               TEMP-PART(8)
               TEMP-PART(9)
               TEMP-PART(10)
               TEMP-PART(11)
               TEMP-PART(12)
               TEMP-PART(13)
               TEMP-PART(14)
               TEMP-PART(15)
               TEMP-PART(16)
               TEMP-PART(17)
               TEMP-PART(18)
               TEMP-PART(19)
               TEMP-PART(20)
               TEMP-PART(21)
               TEMP-PART(22)
               TEMP-PART(23)
               TEMP-PART(24)
               TEMP-PART(25)
               TEMP-PART(26)
               TEMP-PART(27)
               TEMP-PART(28)
               TEMP-PART(29)
               TEMP-PART(30)
           .

           *> CNT計算 (數字判斷)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              IF TEMP-PART(IDX) = SPACES
                 MOVE 2 TO TEMP-PART-CHECK(IDX)
                 EXIT PERFORM
              END-IF
              IF TEMP-PART(IDX) = ","
                CONTINUE
              END-IF

              *> CNT
              MOVE 0 TO CNT-NUM(IDX)
              MOVE "Y" TO CNT-U(IDX)
              MOVE TEMP-PART(IDX) TO TEMP-COL
              MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
              PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TEMP-LEN
                IF TEMP-COL(JDX:1) = ","
                  EXIT PERFORM
                END-IF
              *> 數字判斷
                   IF TEMP-COL(JDX:1) IS NUMERIC OR
                      TEMP-COL(JDX:1) = "-"
                      ADD 1 TO CNT-NUM(IDX)
                   END-IF
              *> 大寫判斷
                   IF NOT(TEMP-COL(JDX:1) IS ALPHABETIC-UPPER OR
                      TEMP-COL(JDX:1) IS NUMERIC OR
                      TEMP-COL(JDX:1) = "-")
                        MOVE "N" TO CNT-U(IDX)
                   END-IF
               END-PERFORM
           END-PERFORM.

      *    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
      *      DISPLAY IDX"IDX:? "FUNCTION TRIM(TEMP-PART(IDX))
      *    END-PERFORM.


      *******************************************************
           *> 剩餘欄位分類
           *> TEMP-COL: 本次迴圈處理
           *> NEXT-COL: 下欄預約
           *> PRE-COL : 累積

           *> TEMP-FLAG: 需重置PER/ 本次納入AFTER
           *> (PER-COL)~(PRE-FLAG)~(TEMP-COL)~(TEMP-FLAG)~
           *> (NEXT-COL)~(NEXT-FLAG)
      *******************************************************
           MOVE SPACES TO TEMP-COL.
           MOVE "N" TO TEMP-FLAG.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              IF TEMP-PART(IDX) = SPACES
                 EXIT PERFORM
              END-IF

              MOVE TEMP-PART(IDX) TO TEMP-COL
              MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
              IF TEMP-COL(TEMP-LEN:1) = ","
                SUBTRACT 1 FROM TEMP-LEN
              END-IF
              MOVE TEMP-COL(1:TEMP-LEN) TO TEMP-PART(IDX)

              IF TEMP-FLAG = "Y" OR DTLS-FLAG NOT = 0
                MOVE SPACES TO PRE-COL PRE-FLAG
                MOVE 0 TO PRE-LEN
                MOVE "N" TO TEMP-FLAG
      *       ELSE
      *         IF IDX NOT = 1
      *           MOVE 99 TO TEMP-PART-CHECK(IDX - 1)
      *           MOVE SPACES TO TEMP-PART(IDX - 1)
      *         END-IF
              END-IF
              MOVE 0 TO DTLS-FLAG IDX-PLUS

              *> NEXT
              MOVE TEMP-PART(IDX + 1) TO NEXT-COL
              MOVE LENGTH OF FUNCTION TRIM(NEXT-COL) TO NEXT-LEN
              MOVE "N" TO NEXT-FLAG
              IF NEXT-COL(NEXT-LEN:1) = ","
                SUBTRACT 1 FROM NEXT-LEN
              END-IF


       *>  ====================== 判斷開始 ======================
              *> =================== NUMBER 判斷 ===================
              *> 是否為 單純數字
              IF CNT-NUM(IDX) = TEMP-LEN
                MOVE "Y" TO TEMP-FLAG
                MOVE "," TO PRE-FLAG
                *> 若此欄位下1/2格為國家 塞入郵遞區號
                IF TEMP-PART-CHECK(IDX + 1) = 2 OR
                   TEMP-PART-CHECK(IDX + 2) = 2
                  MOVE 1 TO DTLS-FLAG
                ELSE
                  MOVE 9 TO DTLS-FLAG
                END-IF

                 *> 荷蘭郵遞區號: 前半:4個數字，後半:大寫英文*2
                 IF TEMP-LEN = 4 AND
                    NEXT-LEN <= 3 AND
                    NEXT-COL(1:2) IS ALPHABETIC-UPPER AND
                    (DTLS-LF(2) = "NETHERLANDS" OR DTLS-LF(2) = "NLD")
                   MOVE "Y" TO NEXT-FLAG
                   MOVE 1   TO DTLS-FLAG
                 END-IF
              END-IF

              *> =================== NUMBER 判斷結束 ===================
              *> =================== FLOOR 判斷 ===================
              MOVE 0 TO CNT-F CNT-FL
              INSPECT NEXT-COL TALLYING CNT-F FOR ALL "F."
              INSPECT NEXT-COL TALLYING CNT-FL FOR ALL "FL."
             IF CNT-NUM(IDX) > 0 AND
                (CNT-F > 0 OR CNT-FL > 0)

                INSPECT TEMP-COL
                  REPLACING ALL "ST" BY SPACES
                INSPECT TEMP-COL
                  REPLACING ALL "ND" BY SPACES
                INSPECT TEMP-COL
                  REPLACING ALL "RD" BY SPACES
                INSPECT TEMP-COL
                  REPLACING ALL "TH" BY SPACES

                INSPECT NEXT-COL
                  REPLACING ALL "F." BY SPACES
                INSPECT NEXT-COL
                  REPLACING ALL "FL." BY SPACES
                
                MOVE SPACES TO TEMP-A
                STRING
                  FUNCTION TRIM(TEMP-COL) DELIMITED BY SIZE
                  FUNCTION TRIM(NEXT-COL) DELIMITED BY SIZE
                  INTO TEMP-A
                END-STRING

                MOVE TEMP-A TO TEMP-COL
                MOVE SPACES TO NEXT-COL
                MOVE "Y" TO NEXT-FLAG TEMP-FLAG
                MOVE 11  TO DTLS-FLAG
              END-IF

              IF TEMP-COL(TEMP-LEN:1) = "F" AND 
                  TEMP-COL(TEMP-LEN - 1:1) IS NUMERIC
                MOVE TEMP-COL(1 : TEMP-LEN - 1) TO TEMP-COL
                MOVE "Y" TO TEMP-FLAG
                MOVE 11  TO DTLS-FLAG
              END-IF

              IF TEMP-COL(TEMP-LEN:2) = "F," AND 
                  TEMP-COL(TEMP-LEN - 2:1) IS NUMERIC
                MOVE TEMP-COL(1 : TEMP-LEN - 2) TO TEMP-COL
                MOVE "Y" TO TEMP-FLAG
                MOVE 11  TO DTLS-FLAG
              END-IF

              IF TEMP-COL(1:1) = "B" AND
                (TEMP-COL(2:TEMP-LEN - 1) IS NUMERIC OR
                (TEMP-COL(TEMP-LEN:1) = "," AND
                 TEMP-COL(2:TEMP-LEN - 2) IS NUMERIC))
                MOVE "Y" TO TEMP-FLAG
                MOVE 11  TO DTLS-FLAG
              END-IF

              *> =================== FLOOR 判斷結束 ===================
              *> =================== 特殊ZIP 判斷 ===================

              *> 是否為 郵遞區號 (英國等地區用)
      *       標準寫法: "XXX XXX" (有空白，分2段)
      *       手寫常見: "XXXXXX"  (無空白，僅1段)
      *      1. 非英國
      *      前半：包含數字*1、總字數3
      *      後半：包含數字*1、總字數3~4
      *      
      *      2. 英國
      *      前半：總字數1~2
      *      前半：總字數3~4，包含數字*1
      *      後半：包含數字*1、總字數3
            IF (DTLS-FLAG NOT = 1 
                AND (TEMP-PART-CHECK(IDX + 1) = 2 OR
                     TEMP-PART-CHECK(IDX + 2) = 2 OR
                     TEMP-PART-CHECK(IDX + 3) = 2)
                AND(
               *> 1. 非英國 標準寫法
               (
               (TEMP-LEN = 3                  AND CNT-NUM(IDX) = 1)) AND
               (NEXT-LEN >= 3 AND NEXT-LEN <= 4 
                                              AND CNT-NUM(IDX + 1) >= 1)

               OR

               *> 2. 英國 標準寫法
               ((DTLS-LF(2) = "GB" OR DTLS-LF(2) = "UK") AND
               ((TEMP-LEN >= 1 AND TEMP-LEN <= 2
                                              AND CNT-NUM(IDX) < 2) OR
                (TEMP-LEN >= 3 AND TEMP-LEN <= 4 
                                              AND CNT-NUM(IDX) = 1)) AND
               (NEXT-LEN >= 3 AND NEXT-LEN <= 4 
                                              AND CNT-NUM(IDX + 1) = 1))
               ))
                MOVE "Y" TO ZIP-FLAG
                *> 前半
                PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TEMP-LEN
                  IF NOT (TEMP-COL(JDX:1) IS ALPHABETIC-UPPER OR
                     TEMP-COL(JDX:1) IS NUMERIC)
                     MOVE "N" TO ZIP-FLAG
                     EXIT PERFORM
                  END-IF
                END-PERFORM

                *> 後半
                PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > NEXT-LEN
                  IF NOT (NEXT-COL(JDX:1) IS ALPHABETIC-UPPER OR
                     NEXT-COL(JDX:1) IS NUMERIC)
                     MOVE "N" TO ZIP-FLAG
                     EXIT PERFORM
                  END-IF
                END-PERFORM

              ELSE
                  MOVE "N" TO ZIP-FLAG
              END-IF

              IF ZIP-FLAG = "Y"
                MOVE "Y" TO TEMP-FLAG NEXT-FLAG
                MOVE 1   TO DTLS-FLAG
                MOVE "," TO PRE-FLAG
              END-IF

            IF (DTLS-FLAG NOT = 1 
                AND (TEMP-PART-CHECK(IDX + 1) = 2 OR
                     TEMP-PART-CHECK(IDX + 2) = 2 OR
                     TEMP-PART-CHECK(IDX + 3) = 2)
                AND CNT-U(IDX) = "Y" AND (
               *> 1. 非英國 手寫常見
               (TEMP-LEN >= 6 AND TEMP-LEN <= 7 AND CNT-NUM(IDX) >= 2)

               OR

               *> 2. 英國 手寫常見
               ((DTLS-LF(2) = "GB" OR DTLS-LF(2) = "UK") AND
               ((TEMP-LEN >= 4 AND TEMP-LEN <= 6 AND CNT-NUM(IDX) < 3)OR
               (TEMP-LEN >= 6 AND TEMP-LEN <= 8 AND CNT-NUM(IDX) = 2)))

               ))
                MOVE "Y" TO TEMP-FLAG
                MOVE 1 TO DTLS-FLAG
                MOVE "," TO PRE-FLAG
              END-IF
              *> =================== 特殊ZIP 判斷結束 ===================
              *> 是否為 號
              IF TEMP-LEN - CNT-NUM(IDX) = 1 AND
                 TEMP-LEN > 1 AND CNT-NUM(IDX) > 1 AND
                 TEMP-COL(TEMP-LEN:1) IS ALPHABETIC-UPPER AND
                 DTLS-FLAG < 11
                MOVE "," TO PRE-FLAG
                MOVE "Y" TO TEMP-FLAG
                MOVE 9 TO DTLS-FLAG
              END-IF

              *> =================== CategoryRule 判斷 ===================
              IF TEMP-FLAG NOT = "Y"
              MOVE FUNCTION UPPER-CASE(TEMP-COL(1:TEMP-LEN)) 
                TO TEMP-UPPER-COL
              IF TEMP-UPPER-COL(TEMP-LEN:1) = ","
                  MOVE TEMP-UPPER-COL(1:TEMP-LEN - 1) TO TEMP-UPPER-COL
              END-IF

              MOVE FUNCTION UPPER-CASE(NEXT-COL(1:NEXT-LEN)) 
                TO NEXT-UPPER-COL
              IF NEXT-UPPER-COL(NEXT-LEN:1) = ","
                  MOVE NEXT-UPPER-COL(1:NEXT-LEN - 1) TO NEXT-UPPER-COL
              END-IF

               *> 全方向
               MOVE "N" TO DIR-FLAG
               PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > DIR-LEN
                 IF NEXT-UPPER-COL = DIR-NAMES(JDX)
                   MOVE "Y" TO DIR-FLAG
                   EXIT PERFORM
                 END-IF
               END-PERFORM

               IF DIR-FLAG = "Y"

                  MOVE ";" TO NEXT-UPPER-COL
                  MOVE "Y" TO TEMP-FLAG NEXT-FLAG
                  MOVE 5   TO DTLS-FLAG

                  *> (**** DIRECTION ROAD) OR
                  *> (**** DE ****)
                  IF FUNCTION UPPER-CASE(TEMP-PART(IDX + 2)(1:4))
                     = "ROAD" OR NEXT-UPPER-COL = "DE"
                    STRING
                      TEMP-COL(1:TEMP-LEN) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      NEXT-COL(1:NEXT-LEN) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      TEMP-PART(IDX + 2)(1:4) DELIMITED BY SIZE
                      INTO TEMP-COL
                    END-STRING
                    MOVE SPACES TO TEMP-PART(IDX + 2) NEXT-COL
                    ADD 1 TO IDX-PLUS
                  END-IF
                END-IF

              *> 以CategoryRule.csv 為準則切割
              PERFORM VARYING JDX FROM 3 BY 1 UNTIL JDX > 16
              PERFORM VARYING KDX FROM 2 BY 1 UNTIL KDX > 40
                MOVE LS-LIST-COL(JDX KDX) TO CHECK-COL
                MOVE LENGTH OF FUNCTION TRIM(CHECK-COL) TO CHECK-LEN
                IF CHECK-COL = SPACES OR CHECK-COL = ALL LOW-VALUES
                  EXIT PERFORM
                END-IF


                *> ===== 羅馬字 判斷 ====
                IF CHECK-COL(1:1) = "-" AND 
                  TEMP-UPPER-COL(TEMP-LEN - CHECK-LEN + 1:
                                 CHECK-LEN) = CHECK-COL

                    MOVE "," TO PRE-FLAG
                    MOVE ";" TO NEXT-UPPER-COL
                    MOVE "Y" TO TEMP-FLAG
                    MOVE JDX TO DTLS-FLAG
                END-IF

                *> ===== 後接字 判斷 ====
                IF CHECK-COL(CHECK-LEN:1) = "-" AND 
                  TEMP-UPPER-COL = CHECK-COL(1:CHECK-LEN - 1)

                    MOVE "," TO PRE-FLAG
                    MOVE ";" TO NEXT-UPPER-COL
                    MOVE "Y" TO TEMP-FLAG
                    STRING
                      TEMP-COL(1:CHECK-LEN - 1) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      NEXT-COL(1:NEXT-LEN) DELIMITED BY SIZE
                      INTO TEMP-COL
                    MOVE SPACES TO NEXT-COL TEMP-PART(IDX + 1)
                    MOVE JDX TO DTLS-FLAG TEMP-PART-CHECK(IDX + 1)
                    MOVE 1 TO IDX-PLUS
                END-IF

                *> ===== 段巷弄號樓室 判斷 =====
                IF JDX >= 6 AND JDX <= 16 AND TEMP-UPPER-COL = CHECK-COL
                  MOVE "Y" TO TEMP-FLAG
                  MOVE JDX TO DTLS-FLAG

                  *> BASEMENT 1 -> B1
                  IF TEMP-UPPER-COL = "BASEMENT"
                    MOVE SPACES TO TEMP-COL
                    STRING
                      "B" DELIMITED BY SIZE
                      NEXT-COL(1:NEXT-LEN) DELIMITED BY SIZE
                      INTO TEMP-COL
                    END-STRING
                    MOVE TEMP-COL TO NEXT-COL
                    MOVE LENGTH OF FUNCTION TRIM(TEMP-COL)
                         TO TEMP-LEN NEXT-LEN
                  END-IF

                  *> 後接字串判斷 特殊狀況(IF)需跳過
                  IF CHECK-COL NOT = "M/F"
                    MOVE "Y" TO NEXT-FLAG
                    MOVE ";" TO NEXT-UPPER-COL
                    IF JDX >= 6 AND JDX <= 11
                          MOVE NEXT-COL TO TEMP-COL *> 省略文字僅留數字
                          MOVE NEXT-LEN TO TEMP-LEN
                          MOVE SPACES TO NEXT-COL *> 省略文字僅留數字
                          MOVE 0 TO NEXT-LEN
                    END-IF
                  END-IF
                END-IF

           *> ===== (NEXT-COL)特定字判斷 =====
                *> 若找到相符內容
                IF NEXT-UPPER-COL = CHECK-COL
                  MOVE "Y" TO TEMP-FLAG
                  MOVE "Y" TO NEXT-FLAG
                  MOVE JDX TO DTLS-FLAG

                  *> 段巷弄號樓室 判斷
                  IF JDX >= 7 AND JDX <= 8 AND
                     FUNCTION TRIM(TEMP-COL) IS NOT NUMERIC
                    MOVE 5 TO DTLS-FLAG
                  END-IF

                  *> FLOOR 字串判斷
                  IF JDX = 11
                    IF CNT-NUM(IDX) > 0 AND NEXT-UPPER-COL = "FLOOR"
                      INSPECT TEMP-COL REPLACING ALL "ST" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "st" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "ND" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "nd" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "RD" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "rd" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "TH" BY SPACES
                      INSPECT TEMP-COL REPLACING ALL "th" BY SPACES
                      MOVE SPACES TO NEXT-COL
                      MOVE 0 TO NEXT-LEN
                    END-IF
                    STRING
                      FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(TEMP-COL) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(NEXT-COL) DELIMITED BY SIZE
                      INTO TEMP-A
                    END-STRING
                    MOVE TEMP-A TO TEMP-COL
                    MOVE SPACES TO PRE-COL NEXT-COL
                    MOVE 0 TO PRE-LEN NEXT-LEN
                  ELSE *> FLOOR 字串判斷 結束

                  *> STREET 後接字串判斷
                  MOVE FUNCTION UPPER-CASE(TEMP-PART(IDX + 2))
                    TO NEXT-UPPER-COL
                    *> 全方向
                  MOVE "N" TO DIR-FLAG
                  PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > DIR-LEN
                    IF NEXT-UPPER-COL = DIR-NAMES(JDX)
                    MOVE "Y" TO DIR-FLAG
                    EXIT PERFORM
                    END-IF
                  END-PERFORM

                  IF DIR-FLAG = "Y"
                       STRING
                          NEXT-COL(1:NEXT-LEN) DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(TEMP-PART(IDX + 2))
                            DELIMITED BY SIZE
                          INTO NEXT-COL
                       END-STRING

                       MOVE 1 TO IDX-PLUS

                    END-IF
                  END-IF

                  EXIT PERFORM
                END-IF
           *> ===== (NEXT-COL)特定字判斷 完成 =====
              END-PERFORM
              END-PERFORM
              END-IF
              *> =================== CategoryRule 判斷結束 ===================

              *> 尚未分類完成 但本身便帶有","
              *> 特殊處理欄位/ AFTER-DATA納入主判斷
              IF TEMP-COL(TEMP-LEN + 1:1) = "," AND DTLS-FLAG = 0
      *         DISPLAY "OTHER!!!!" 
      *           FUNCTION TRIM(PRE-COL)"/ "
      *           FUNCTION TRIM(TEMP-COL)"/ "
      *           FUNCTION TRIM(NEXT-COL)"/ "
      *           MATCH-NEW"/ "IDX"/ "TEMP-PART-CHECK(MATCH-NEW)

                MOVE "Y" TO TEMP-FLAG

                *> 樓層 + 大樓
                IF IDX NOT = 1 AND TEMP-PART-CHECK(MATCH-NEW) = 11
                  MOVE 14 TO DTLS-FLAG
                END-IF

                *> 若尚未填入 路以上之欄位
                IF DTLS-FLAG = 0 AND DTLS-LF(5) = SPACES AND 
                TEMP-PART-CHECK(MATCH-NEW) > 5 OR
                TEMP-PART-CHECK(MATCH-NEW) <= 2 OR
                   MATCH-NEW = 0
                  *> STREET 後接字串判斷
                  MOVE FUNCTION UPPER-CASE(NEXT-COL) TO NEXT-UPPER-COL
                  IF NEXT-UPPER-COL(NEXT-LEN:1) = ","
                      MOVE NEXT-UPPER-COL(1:NEXT-LEN - 1) 
                        TO NEXT-UPPER-COL
                  END-IF

                  *> 全方向
                  PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > DIR-LEN
                    IF NEXT-UPPER-COL = DIR-NAMES(JDX)
                    MOVE "Y" TO NEXT-FLAG
                    EXIT PERFORM
                    END-IF
                  END-PERFORM

                  MOVE 5 TO DTLS-FLAG
                END-IF

                  PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 1
                    IF TEMP-PART-CHECK(JDX - 1) = 0
                      MOVE SPACES TO TEMP-PART(JDX - 1)
                      MOVE 99 TO TEMP-PART-CHECK(JDX - 1)
                    END-IF
                  END-PERFORM
                
              END-IF

       *>  ====================== 判斷結束 ======================
           *> NEXT-FLAG
              IF NEXT-FLAG = "Y"
                   STRING
                     FUNCTION TRIM(TEMP-COL) DELIMITED BY SIZE
                     " " DELIMITED BY SIZE
                     FUNCTION TRIM(NEXT-COL) DELIMITED BY SIZE
                     INTO TEMP-COL
                   END-STRING

                   COMPUTE TEMP-LEN = TEMP-LEN + NEXT-LEN + 1
                   MOVE SPACES TO TEMP-PART(IDX) TEMP-PART(IDX + 1)
                   MOVE DTLS-FLAG TO TEMP-PART-CHECK(IDX)
                                     TEMP-PART-CHECK(IDX + 1)
                   ADD 1 TO IDX
              END-IF

              IF PRE-COL(PRE-LEN:1) = "," OR
                 IDX = 1 OR PRE-LEN = 0
                MOVE SPACES TO PRE-FLAG
              END-IF

           *> CHECK PRE-FLAG
           IF (PRE-COL(PRE-LEN:1) = "," OR PRE-FLAG = ",") AND 
               DTLS-FLAG NOT = 0 AND PRE-LEN > 0
             IF DTLS-LF(14) = SPACES AND TEMP-PART-CHECK(MATCH-NEW) = 11
               MOVE PRE-COL TO DTLS-LF(14)
             ELSE
             IF DTLS-LF(5) = SPACES
               MOVE PRE-COL TO DTLS-LF(5)
             ELSE
               MOVE PRE-COL TO TEMP-PART(IDX - 1)

               *> 清理TEMP-PART
               IF IDX > 2
                 PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 2
                   IF TEMP-PART-CHECK(JDX - 2) = 0
                     MOVE SPACES TO TEMP-PART(JDX - 2)
                     MOVE 99 TO TEMP-PART-CHECK(JDX - 2)
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
               MOVE SPACES TO PRE-COL PRE-FLAG
               MOVE 0 TO PRE-LEN
             END-IF
           END-IF

           *> 串聯
              STRING
                FUNCTION TRIM(PRE-COL)  DELIMITED BY SIZE
                FUNCTION TRIM(PRE-FLAG) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(TEMP-COL) DELIMITED BY SIZE
                INTO PRE-COL
              END-STRING
              COMPUTE PRE-LEN = PRE-LEN + TEMP-LEN + 1

           *> PRE-FLAG
              IF AFTER-DATA(LENGTH OF FUNCTION TRIM(AFTER-DATA):1) = ","
                 OR IDX = 1
                MOVE SPACES TO PRE-FLAG
              END-IF

           *> DTLS-FLAG判斷
           IF DTLS-FLAG NOT = 0
             MOVE FUNCTION TRIM(PRE-COL) TO PRE-COL
             MOVE LENGTH OF FUNCTION TRIM(PRE-COL) TO PRE-LEN
             IF PRE-COL(PRE-LEN:1) = ","
               MOVE PRE-COL(1:PRE-LEN - 1) TO PRE-COL
             END-IF

             IF DTLS-LF(DTLS-FLAG) = SPACES
               MOVE FUNCTION TRIM(PRE-COL) TO DTLS-LF(DTLS-FLAG)
               MOVE DTLS-FLAG TO TEMP-PART-CHECK(IDX)
               MOVE IDX TO MATCH-NEW
               MOVE SPACES TO TEMP-PART(IDX)
             END-IF

             *> 清理TEMP-PART
             IF IDX > 2
               PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 2
                 IF TEMP-PART-CHECK(JDX - 2) = 0
                   MOVE SPACES TO TEMP-PART(JDX - 2)
                   MOVE 99 TO TEMP-PART-CHECK(JDX - 2)
                 END-IF
               END-PERFORM
             END-IF
           END-IF

           *> 納入 TEMP-PART(IDX)
              IF TEMP-FLAG = "Y" AND DTLS-FLAG  = 0
                IF TEMP-COL(TEMP-LEN + 1:1) = ","
                  STRING
                    FUNCTION TRIM(AFTER-DATA) DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
                    INTO TEMP-PART(IDX)
                  END-STRING
                ELSE
                  STRING
                    FUNCTION TRIM(AFTER-DATA) DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
                    "," DELIMITED BY SIZE
                    INTO TEMP-PART(IDX)
                  END-STRING
                END-IF
                MOVE 98 TO TEMP-PART-CHECK(IDX)
           END-IF

      *    *> ","插入判斷結果
      *    DISPLAY "IDX : "IDX "/ " TEMP-FLAG
      *    DISPLAY "CNT : "CNT-NUM(IDX)"/ "CNT-NUM(IDX + 1)
      *    DISPLAY "PRE : "FUNCTION TRIM(PRE-COL)"/ "PRE-LEN"/ "PRE-FLAG
      *                 "/"FUNCTION TRIM(TEMP-PART(IDX - 1))
      *    DISPLAY "TEMP: "
      *            FUNCTION TRIM(TEMP-COL) "/ " TEMP-LEN"/ "CNT-NUM(IDX)
      *            "/"FUNCTION TRIM(TEMP-PART(IDX))"/"
      *            TEMP-PART-CHECK(IDX)
      *    DISPLAY "MIX : "NEXT-FLAG
      *    DISPLAY "NEXT: "
      *          FUNCTION TRIM(NEXT-COL)"/ "NEXT-LEN"/ "CNT-NUM(IDX + 1)
      *    DISPLAY "DTLS:"DTLS-FLAG"/ "FUNCTION TRIM(DTLS-LF(DTLS-FLAG))
      *    DISPLAY FUNCTION TRIM(AFTER-DATA)"/ "MATCH-NEW
      *    DISPLAY "------------- ------------- -------------"

           ADD IDX-PLUS TO IDX
           EVALUATE TEMP-PART-CHECK(IDX)
             WHEN 3
               MOVE IDX TO OTHER-CITY
             WHEN 4
               MOVE IDX TO OTHER-DISTRICT
             WHEN 5
               MOVE IDX TO OTHER-STREET
             WHEN 16
               MOVE IDX TO OTHER-PROVINCE
      *         WHEN OTHER
           END-EVALUATE
           END-PERFORM.

      *> ===================== OTHEER =====================
      *> ============= 依照相對位置選擇插入欄位 =============
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 16
             IF TEMP-PART-CHECK(IDX) < 30 AND 
                TEMP-PART-CHECK(IDX) > 0
                MOVE TEMP-PART-CHECK(IDX) TO OTHER-PRE
             END-IF

             IF TEMP-PART(IDX) NOT = SPACES
               MOVE "N" TO OTHER-FLAG
               MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO TEMP-COL
               MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
               IF TEMP-COL(TEMP-LEN:1) = ","
                 SUBTRACT 1 FROM TEMP-LEN
                 MOVE TEMP-COL(1:TEMP-LEN) TO TEMP-COL
               END-IF
             
             *> ============ of 開頭 ============
               IF FUNCTION UPPER-CASE(TEMP-COL(1:2)) = "OF"
               PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 1
                 IF TEMP-PART-CHECK(JDX - 1) NOT = 0 AND
                    TEMP-PART-CHECK(JDX - 1) NOT = 99
                      STRING
                        FUNCTION TRIM(DTLS-LF(TEMP-PART-CHECK(JDX - 1)))
                          DELIMITED BY SIZE
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TEMP-COL) DELIMITED BY SIZE
                      INTO DTLS-LF(TEMP-PART-CHECK(JDX - 1))
                      END-STRING
                      MOVE "Y" TO OTHER-FLAG
                      EXIT PERFORM
                 END-IF
               END-PERFORM
               END-IF

      *      DISPLAY IDX" CHECK: "FUNCTION TRIM(TEMP-COL)"/"TEMP-LEN
      *      "/STREE-CITY-DIS-PRO: "OTHER-STREET"/"
      *      OTHER-CITY"/"OTHER-DISTRICT"/"OTHER-PROVINCE
             *> ============ CITY ============
               IF OTHER-FLAG = "N" AND OTHER-CITY = 0
                  AND IDX > OTHER-STREET
                 MOVE TEMP-COL TO DTLS-LF(3)
                 MOVE "Y" TO OTHER-FLAG
                 MOVE IDX TO OTHER-CITY
               END-IF

             *> ============ PROVINCE ============
               IF OTHER-FLAG = "N" AND OTHER-PROVINCE = 0
                  AND IDX > OTHER-CITY
                 MOVE TEMP-COL TO DTLS-LF(16)
                 MOVE "Y" TO OTHER-FLAG
               ELSE
                 IF OTHER-FLAG = "N" AND OTHER-PROVINCE = 0
                   MOVE DTLS-LF(3) TO DTLS-LF(16)
                   MOVE TEMP-COL TO DTLS-LF(16)
                   MOVE "Y" TO OTHER-FLAG
                 END-IF
               END-IF

             *> ============ STREET ============
               IF OTHER-FLAG = "N" AND OTHER-STREET = 0
                  AND ((OTHER-CITY > 0 AND IDX < OTHER-CITY) OR
                  (OTHER-PROVINCE > 0 AND IDX < OTHER-PROVINCE))
                 MOVE TEMP-COL TO DTLS-LF(5)
                 MOVE "Y" TO OTHER-FLAG
               ELSE
                 IF OTHER-FLAG = "N" AND OTHER-PROVINCE = 0
                    AND OTHER-PRE < IDX
                   MOVE TEMP-COL TO DTLS-LF(5)
                   MOVE "Y" TO OTHER-FLAG
                 END-IF
               END-IF

             *> ============ VILLAGE -> OTHER ============
               IF OTHER-FLAG = "N" AND DTLS-LF(15) = SPACES
                   MOVE TEMP-COL TO DTLS-LF(15)
                   MOVE "Y" TO OTHER-FLAG
               END-IF
               IF OTHER-FLAG = "N"
                   MOVE TEMP-COL TO DTLS-LF(18)
               END-IF

             END-IF
           
           END-PERFORM.





           *> OTHER 欄位
           MOVE AFTER-DATA(1:TEMP-LEN) TO DTLS-LF(18). 


      *> ===================== REBUILD =====================
           MOVE SPACES TO DTLS-LF(23).
      *>   設定順序
      *>   11[FLOOR] → 13[ROOM] → 14[BUILDING] → 9[NUMBER] → 
      *>   8[ALLEY] → 7[LANE] → 6[SEC] → 5[SREET] → 
      *>   18[OTHER] → 15[VILLAGE] →
      *>   4[DISTRICT] → 3[CITY] → 16[PROVINCE] → 17[STATE] → 
      *>   1[ZIP] → 2[COUNTRY]
           MOVE 11 TO CNT-NUM(1).
           MOVE 13 TO CNT-NUM(2).
           MOVE 14 TO CNT-NUM(3).
           MOVE  9 TO CNT-NUM(4).

           MOVE  8 TO CNT-NUM(5).
           MOVE  7 TO CNT-NUM(6).
           MOVE  6 TO CNT-NUM(7).
           MOVE  5 TO CNT-NUM(8).

           MOVE 18 TO CNT-NUM(9).
           MOVE 15 TO CNT-NUM(10).

           MOVE  4 TO CNT-NUM(11).
           MOVE  3 TO CNT-NUM(12).
           MOVE 16 TO CNT-NUM(13).
           MOVE 17 TO CNT-NUM(14).

           MOVE  1 TO CNT-NUM(15).
           MOVE  2 TO CNT-NUM(16).

           *> 串聯
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 16
             IF DTLS-LF(CNT-NUM(IDX)) NOT = SPACES
               MOVE FUNCTION TRIM(DTLS-LF(23)) TO TEMP-A
               MOVE FUNCTION TRIM(DTLS-LF(CNT-NUM(IDX))) TO TEMP-B
               MOVE SPACES TO TEMP-COL

               IF CNT-NUM(IDX) = 11 AND TEMP-B(1:1) IS NUMERIC
                 STRING
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   " Floor" DELIMITED BY SIZE
                   INTO TEMP-COL
                 END-STRING
                 MOVE TEMP-COL TO TEMP-B
               END-IF

               IF CNT-NUM(IDX) = 8 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Aly. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TEMP-COL
                 END-STRING
                 MOVE TEMP-COL TO TEMP-B
               END-IF

               IF CNT-NUM(IDX) = 7 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Ln. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TEMP-COL
                 END-STRING
                 MOVE TEMP-COL TO TEMP-B
               END-IF

               IF CNT-NUM(IDX) = 6 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Sec. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TEMP-COL
                 END-STRING
                 MOVE TEMP-COL TO TEMP-B
               END-IF

               STRING
                 FUNCTION TRIM(TEMP-A) DELIMITED BY SIZE
                 ", " DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                 INTO DTLS-LF(23)
               END-STRING
             END-IF
           END-PERFORM.
           MOVE DTLS-LF(23)(3:LENGTH OF FUNCTION TRIM(DTLS-LF(23)) - 2)
             TO DTLS-LF(23).


      *******************************************************
      *> 處理錯誤資料
      *******************************************************
       ERROR-SECTION.
           MOVE "N" TO ERROR-FLAG.
           MOVE "PLEASE ENTER" TO ERROR-TEMP.
           MOVE SPACES TO COMMA-FLAG.
           
           *> ZIP 為空值
           IF DTLS-LF(1) = SPACES
             STRING 
               FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " POSTAL CODE" DELIMITED BY SIZE
               INTO ERROR-TEMP
             END-STRING
             MOVE "Y" TO ERROR-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.
           
           *> COUNTRY 為空值
           IF DTLS-LF(2) = SPACES
             STRING 
               FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " COUNTRY" DELIMITED BY SIZE
               INTO ERROR-TEMP
             END-STRING
             MOVE "Y" TO ERROR-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.
           
           *> CITY 為空值 PROVINCE 皆為空值
           IF DTLS-LF(3) = SPACES  AND DTLS-LF(16) = SPACES
             STRING 
               FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " CITY OR PROVINCE" DELIMITED BY SIZE
               INTO ERROR-TEMP
             END-STRING
             MOVE "Y" TO ERROR-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.
           
           *> OTHER 有值: PARSING FAILED. PLEASE CHECK INPUT
           IF DTLS-LF(18) NOT = SPACES
             MOVE "PARSING FAILED. PLEASE CHECK INPUT" TO ERROR-TEMP
             MOVE "Y" TO ERROR-FLAG
           END-IF.


           *> 錯誤分析: 若 TRIM(DTLS-LF(IDX)) 字數 > 35 -> ERROR
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
             IF LENGTH OF FUNCTION TRIM(DTLS-LF(IDX)) > 35
               MOVE "ADDRESS DATA IS TOO LONG" TO ERROR-TEMP
             END-IF
           END-PERFORM
    
           *> 錯誤分析: 若包含特殊字體 -> ERROR
           MOVE "N" TO CHARACTERS-FLAG
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
             LENGTH OF FUNCTION TRIM(ORIGIN-DATA)

             MOVE ORIGIN-DATA(IDX:1) TO WS-CH
             COMPUTE WS-CODE = FUNCTION ORD(WS-CH)
             IF WS-CODE < 32 OR WS-CODE > 126
               MOVE "Y" TO ERROR-FLAG
               MOVE "CONTAINS INVALID CHARACTERS" TO ERROR-TEMP
               EXIT PERFORM
             ELSE
               INSPECT ALLOWED-CH TALLYING WS-CODE FOR ALL WS-CH
               IF NOT((WS-CH >= "0" AND WS-CH <= "9") OR 
                  (WS-CH >= "A" AND WS-CH <= "Z") OR
                  (WS-CH >= "a" AND WS-CH <= "z") OR
                  WS-CH = SPACE OR
                  WS-CODE > 0)
                   MOVE "Y" TO ERROR-FLAG
                   MOVE "CONTAINS INVALID CHARACTERS" TO ERROR-TEMP
                   EXIT PERFORM
               END-IF
             END-IF
           END-PERFORM.

           IF ERROR-FLAG = "Y"
             STRING 
               FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
               "." DELIMITED BY SIZE
               INTO ERROR-TEMP
             END-STRING
             
             MOVE ERROR-TEMP TO DTLS-LF(19)
           ELSE
             MOVE SPACES TO DTLS-LF(19)
           END-IF.


      *******************************************************
      *> 輸出結果
      *******************************************************
       OUTPUT-SECTION.
      *    DISPLAY "******OTHER:     "FUNCTION TRIM(DTLS-LF(18))
      *    DISPLAY "******REBUILD:     "FUNCTION TRIM(DTLS-LF(23))

      *    IF AFTER-DATA NOT = SPACES
      *          DISPLAY "MOTO : "FUNCTION TRIM(BEFORE-DATA)
      *          DISPLAY "OTHER: "FUNCTION TRIM(PROCESSING-DATA)
      *          DISPLAY "KEKKA: "FUNCTION TRIM(AFTER-DATA)
      *          DISPLAY "================================="
      *    END-IF.

      *    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 23
      *      DISPLAY IDX" IDX: "FUNCTION TRIM(DTLS-LF(IDX))
      *      "/ TEMP: "FUNCTION TRIM(TEMP-PART(IDX)) "/ "
      *                TEMP-PART-CHECK(IDX)
      *    END-PERFORM.


      *******************************************************
      *> 結束處理
      *******************************************************
           EXIT PROGRAM.
       END PROGRAM FORMATTER-ADDRESS.
