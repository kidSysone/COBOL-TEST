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
           05 CNT              PIC 9(3) OCCURS 30 TIMES.
           05 CNT-U            PIC X OCCURS 30 TIMES.
           05 TEMP-A           PIC X(100).
           05 TEMP-B           PIC X(100).
           05 TEMP-COL         PIC X(500).
           05 TEMP-COL-2       PIC X(500).
           05 TEMP-LEN         PIC 999.
           05 TEMP-FLAG        PIC XX VALUE "Y".
           05 ZIP-FLAG         PIC X. *> 英國等地區之郵遞區號
           05 ROAD-FLAG        PIC X. *> 道路名稱

           05 CNT-F            PIC 9.
           05 CNT-FL           PIC 9.
           05 CNT-FLOOR        PIC 9.

           05 PRE-COL          PIC X(100).
           05 PRE-LEN          PIC 999.
           05 PRE-FLAG         PIC XX.

           05 NEXT-COL         PIC X(100).
           05 NEXT-UPPER-COL   PIC X(100).
           05 NEXT-LEN         PIC 999.
           05 NEXT-FLAG        PIC XX.

           05 CHECK-COL        PIC X(100). *> LS-LIST-COL

       01 IDX PIC 99999.
       01 JDX PIC 999.
       01 KDX PIC 99.
       01 FOUND-JDX PIC 999.
       01 END-JDX PIC 999.


      *******************************************************
      *> 資料部、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-FORMATTER.
           05 BEFORE-DATA PIC X(500). *> 格式化讀取資料
           05 AFTER-DATA  PIC X(500). *> 格式化回傳資料
           05 DTLS-LF     PIC X(100) OCCURS 18 TIMES. *> 地址欄位

       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES. 

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-LIST-REC LS-FORMATTER.
      *PROCEDURE DIVISION.
       MAIN SECTION.

           *> TEMP-PART 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              MOVE SPACES TO TEMP-PART(IDX) 
           END-PERFORM.

           *> AFTER-DATA 初期化
           MOVE SPACES TO AFTER-DATA.

           *> BEFORE-DATA 初期化
           MOVE FUNCTION TRIM(BEFORE-DATA) TO BEFORE-DATA

           *> DTLS-LF 初期化
            PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                MOVE SPACES TO DTLS-LF(IDX) 
            END-PERFORM.

           *> 迴避複數空格的狀況
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

           MOVE SPACES TO BEFORE-DATA.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF TEMP-PART(IDX) NOT = SPACES
               STRING
                 FUNCTION TRIM(BEFORE-DATA) DELIMITED BY SIZE
                 " " DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-PART(IDX)) DELIMITED BY SIZE
                 INTO BEFORE-DATA
               END-STRING
             END-IF
           END-PERFORM.
           MOVE FUNCTION TRIM(BEFORE-DATA) TO BEFORE-DATA.

           *> TEMP-PART 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              MOVE SPACES TO TEMP-PART(IDX) 
           END-PERFORM.

           *> "," -> ", "
           MOVE LENGTH OF FUNCTION TRIM(BEFORE-DATA) TO TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TEMP-LEN
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
                    ADD 1 TO TEMP-LEN
               END-IF
           END-PERFORM.


           *> 抽出 COUNTRY
           MOVE FUNCTION UPPER-CASE(BEFORE-DATA) TO TEMP-COL.
           MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 500
              IF LS-COUNTRY-COL(IDX) = SPACES
                EXIT PERFORM
              END-IF
              IF LS-COUNTRY-COL(IDX)(1:1) = '"'
                MOVE LS-COUNTRY-COL(IDX)(2:
                  LENGTH OF FUNCTION TRIM(LS-COUNTRY-COL(IDX)) - 2) 
                  TO NEXT-COL
              ELSE
                MOVE LS-COUNTRY-COL(IDX) TO NEXT-COL
              END-IF
              MOVE LENGTH OF FUNCTION TRIM(NEXT-COL) TO NEXT-LEN
              *> 若找到相符內容
              IF TEMP-COL(TEMP-LEN - NEXT-LEN + 1:NEXT-LEN) = NEXT-COL 
                IF BEFORE-DATA(TEMP-LEN - NEXT-LEN - 1:1) = ","
                  MOVE 2 TO PRE-LEN
                ELSE  
                  MOVE 1 TO PRE-LEN
                END-IF
                MOVE BEFORE-DATA(TEMP-LEN - NEXT-LEN + 1:NEXT-LEN) 
                  TO DTLS-LF(2)
                MOVE BEFORE-DATA(1:TEMP-LEN - NEXT-LEN - PRE-LEN)
                  TO BEFORE-DATA
                EXIT PERFORM
              END-IF
           END-PERFORM.


           *> CITY 補上, 
           *> BEFORE-DATA 中是否有 CITY
           *> TEMP-COL = BEFORE-DATA大寫, TEMP-LEN = BEFORE-DATA長度
           MOVE FUNCTION UPPER-CASE(BEFORE-DATA) TO TEMP-COL.
           MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 50000
              IF LS-CITY-COL(IDX) = SPACES
                EXIT PERFORM
              END-IF
              MOVE LS-CITY-COL(IDX) TO NEXT-COL
              MOVE LENGTH OF FUNCTION TRIM(NEXT-COL) TO NEXT-LEN
              MOVE 0 TO FOUND-JDX
              PERFORM VARYING JDX FROM 1 BY 1
                UNTIL JDX > TEMP-LEN - NEXT-LEN + 1

                  IF TEMP-COL(JDX:NEXT-LEN) = NEXT-COL(1:NEXT-LEN)
                   *> 確認該單字前後皆為空白後才能進入
                   IF (
             JDX > 1 AND TEMP-COL(JDX - 1:1) = SPACES
             AND
             (JDX < TEMP-LEN AND TEMP-COL(JDX + NEXT-LEN:1) = SPACES OR
              JDX < TEMP-LEN AND TEMP-COL(JDX + NEXT-LEN:1) = ",")
             )
                      MOVE JDX TO FOUND-JDX
                      EXIT PERFORM
                     END-IF
                  END-IF
              END-PERFORM

              *> 若找到相符內容
              IF FOUND-JDX > 0
                MOVE "N" TO ROAD-FLAG
                *> CITY + "CITY"字串
                IF TEMP-COL(JDX + NEXT-LEN + 1:4) = "CITY"
                  ADD 5 TO NEXT-LEN
                END-IF

               *> 準備插入","
                COMPUTE END-JDX = FOUND-JDX + NEXT-LEN - 1
                 *> 前面,
                 IF BEFORE-DATA(FOUND-JDX - 2: 1) NOT = ","
                   MOVE ", " TO TEMP-FLAG
                   MOVE 2 TO PRE-LEN
                 ELSE  
                   MOVE ", " TO TEMP-FLAG
                   MOVE 3 TO PRE-LEN
                 END-IF

                 *> 後面,
                 IF BEFORE-DATA(END-JDX + 1:1) NOT = ","
                   MOVE ", " TO NEXT-FLAG
                   MOVE 2 TO NEXT-LEN
                 ELSE  
                   MOVE ", " TO NEXT-FLAG
                   MOVE 3 TO NEXT-LEN
                 END-IF

                 MOVE BEFORE-DATA(FOUND-JDX:END-JDX - FOUND-JDX + 1)
                      TO TEMP-COL-2

                *> CITY + TEMP-A(.)
                UNSTRING BEFORE-DATA(END-JDX + NEXT-LEN:TEMP-LEN)
                  DELIMITED BY ALL " "
                  INTO TEMP-A TEMP-B
                END-UNSTRING

                IF TEMP-A(1:2) = "St" OR TEMP-A(1:2) = "Rd"
                OR TEMP-A(1:2) = "Dr" 
                OR TEMP-A(1:3) = "Ave" OR TEMP-A(1:3) = "Riv"
                OR TEMP-A(1:4) = "Blvd"
                   *> TEMP-A 整理
                   IF TEMP-A(LENGTH OF FUNCTION TRIM(TEMP-A) : 1) = ","
                     ADD 1 TO END-JDX
                     UNSTRING TEMP-A DELIMITED BY ALL ","
                       INTO TEMP-A
                     END-UNSTRING
                   END-IF
                   IF TEMP-A(LENGTH OF FUNCTION TRIM(TEMP-A) : 1) = "."
                     MOVE " " TO ROAD-FLAG
                   ELSE
                     MOVE "." TO ROAD-FLAG
                   END-IF

                   DISPLAY "TEMP-A: " FUNCTION TRIM(TEMP-A)
                   COMPUTE END-JDX = END-JDX + 
                                  LENGTH OF FUNCTION TRIM(TEMP-A) + 1
                  STRING 
                    FUNCTION TRIM(TEMP-COL-2) DELIMITED BY SIZE
                    " "
                    FUNCTION TRIM(TEMP-A) DELIMITED BY SIZE
                    ROAD-FLAG DELIMITED BY SPACE
                    INTO TEMP-B
                  END-STRING
       
                   MOVE ", " TO NEXT-FLAG
                   MOVE 2 TO NEXT-LEN
                   MOVE TEMP-B TO TEMP-COL-2
                END-IF

      *        *> 確認分割狀態
      *        DISPLAY "------------------------------------"
      *        DISPLAY "JDX?? "FOUND-JDX"/ "END-JDX
      *        DISPLAY "LEN?? "TEMP-LEN"/ "PRE-LEN"/ "NEXT-LEN
      *        DISPLAY "HAI?? "FUNCTION TRIM(BEFORE-DATA)
      *        DISPLAY "HAI?? "BEFORE-DATA(1:FOUND-JDX - PRE-LEN)
      *        DISPLAY "HAI?? "TEMP-FLAG
      *        DISPLAY "HAI?? "FUNCTION TRIM(TEMP-COL-2)
      *        DISPLAY "HAI?? "NEXT-FLAG
      *        DISPLAY "HAI?? "BEFORE-DATA(END-JDX + NEXT-LEN:TEMP-LEN)

                *> (1 ~ FOUND-JDX) + TEMP-COL-2 + (END-JDX+* ~ END)
               STRING
                 BEFORE-DATA(1:FOUND-JDX - PRE-LEN) DELIMITED BY SIZE
                 TEMP-FLAG DELIMITED BY SIZE
                 BEFORE-DATA(END-JDX + NEXT-LEN:TEMP-LEN)
                   DELIMITED BY SIZE
                 INTO TEMP-COL
               END-STRING
               MOVE TEMP-COL TO BEFORE-DATA
               IF ROAD-FLAG NOT = "N"
                 MOVE FUNCTION TRIM(TEMP-COL-2) TO DTLS-LF(5)
               ELSE
                 MOVE FUNCTION TRIM(TEMP-COL-2) TO DTLS-LF(3)
                 EXIT PERFORM
               END-IF

              END-IF
           END-PERFORM.



           *> 資料分類
           UNSTRING BEFORE-DATA
             DELIMITED BY SPACE
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
           .

           *> 簡寫地名補上.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              IF TEMP-PART(IDX) = SPACES
                 EXIT PERFORM
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
                     ".," DELIMITED BY SIZE
                     INTO TEMP-PART(IDX)
                 END-STRING
              END-IF

              *> CNT
              MOVE 0 TO CNT(IDX)
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
                      ADD 1 TO CNT(IDX)
                   END-IF
              *> 大寫判斷
                   IF NOT(TEMP-COL(JDX:1) IS ALPHABETIC-UPPER OR
                      TEMP-COL(JDX:1) IS NUMERIC OR
                      TEMP-COL(JDX:1) = "-")
                        MOVE "N" TO CNT-U(IDX)
                   END-IF
               END-PERFORM
           END-PERFORM.


      *******************************************************
           *> 補上,
           *> TEMP-COL: 本次迴圈處理
           *> NEXT-COL: 下欄預約
           *> PRE-COL : 累積

           *> TEMP-FLAG: 需重置PER/ 本次納入AFTER
           *> (PER-COL)~(PRE-FLAG)~(TEMP-COL)~(TEMP-FLAG)~
           *> (NEXT-COL)~(NEXT-FLAG)
      *******************************************************
           MOVE SPACES TO TEMP-COL.
           MOVE "Y" TO TEMP-FLAG.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
              IF TEMP-PART(IDX) = SPACES
                 EXIT PERFORM
              END-IF

              MOVE TEMP-PART(IDX) TO TEMP-COL
              MOVE LENGTH OF FUNCTION TRIM(TEMP-COL) TO TEMP-LEN
              IF TEMP-COL(TEMP-LEN:1) = ","
                SUBTRACT 1 FROM TEMP-LEN
              END-IF

              IF TEMP-FLAG = "Y"
                MOVE SPACES TO PRE-COL PRE-FLAG
                MOVE 0 TO PRE-LEN
                MOVE "N" TO TEMP-FLAG
              END-IF

              *> NEXT
              MOVE TEMP-PART(IDX + 1) TO NEXT-COL
              MOVE LENGTH OF FUNCTION TRIM(NEXT-COL) TO NEXT-LEN
              MOVE "N" TO NEXT-FLAG
              IF NEXT-COL(NEXT-LEN:1) = ","
                SUBTRACT 1 FROM NEXT-LEN
              END-IF


       *>  ====================== 判斷開始 ======================
              *> 是否為 單純數字
              IF CNT(IDX) = TEMP-LEN

                 *> 荷蘭郵遞區號: 前半:4個數字，後半:大寫英文*2
                 IF TEMP-LEN = 4 AND
                    NEXT-LEN <= 3 AND
                    FUNCTION TRIM(NEXT-COL(1:2)) IS ALPHABETIC-UPPER AND
                    (DTLS-LF(2) = "NETHERLANDS" OR DTLS-LF(2) = "NLD")
                   MOVE "Y" TO NEXT-FLAG
                 END-IF

                MOVE "Y" TO TEMP-FLAG
                MOVE "," TO PRE-FLAG
              END-IF

              *> 下一段是否為 樓層 FLOOR,FL.,F.
              MOVE 0 TO CNT-F CNT-FL CNT-FLOOR
              INSPECT NEXT-COL TALLYING CNT-F FOR ALL "F."
              INSPECT NEXT-COL TALLYING CNT-FL FOR ALL "FL."
              INSPECT NEXT-COL TALLYING CNT-FLOOR FOR ALL "FLOOR"
             IF CNT-F > 0 OR CNT-FL > 0 OR CNT-FLOOR > 0
                MOVE "Y" TO NEXT-FLAG
                MOVE "Y" TO TEMP-FLAG
              END-IF

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
            IF (
               *> 1. 非英國 標準寫法
               (
               (TEMP-LEN = 3                    AND CNT(IDX) = 1)) AND
               (NEXT-LEN >= 3 AND NEXT-LEN <= 4 AND CNT(IDX + 1) >= 1)

               OR

               *> 2. 英國 標準寫法
               ((DTLS-LF(2) = "UNITED KINGDOM" OR DTLS-LF(2) = "UK") AND
               ((TEMP-LEN >= 1 AND TEMP-LEN <= 2 AND CNT(IDX) < 2) OR
               (TEMP-LEN >= 3 AND TEMP-LEN <= 4 AND CNT(IDX) = 1)) AND
               (NEXT-LEN >= 3 AND NEXT-LEN <= 4 AND CNT(IDX + 1) = 1))

               )
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
                MOVE "Y" TO TEMP-FLAG
                MOVE "Y" TO NEXT-FLAG
                MOVE "," TO PRE-FLAG
              END-IF

            IF ( CNT-U(IDX) = "Y" AND (
               *> 1. 非英國 手寫常見
               (TEMP-LEN >= 6 AND TEMP-LEN <= 7 AND CNT(IDX) >= 2)

               OR

               *> 2. 英國 手寫常見
               ((DTLS-LF(2) = "UNITED KINGDOM" OR DTLS-LF(2) = "UK") AND
               ((TEMP-LEN >= 4 AND TEMP-LEN <= 6 AND CNT(IDX) < 3) OR
               (TEMP-LEN >= 6 AND TEMP-LEN <= 8 AND CNT(IDX) = 2)))

               ))
                MOVE "Y" TO TEMP-FLAG
                MOVE "," TO PRE-FLAG
              END-IF

              *> 是否為 州
              IF FUNCTION TRIM(TEMP-COL) IS ALPHABETIC-UPPER AND
                 (TEMP-LEN >= 2 OR TEMP-LEN <= 3)
                MOVE "Y" TO TEMP-FLAG
                MOVE "," TO PRE-FLAG
              END-IF

              *> 是否為 縮寫
              IF TEMP-COL(TEMP-LEN:1) = "."
                IF CNT(IDX + 1) = NEXT-LEN
                  MOVE "Y" TO NEXT-FLAG
                END-IF
                MOVE "Y" TO TEMP-FLAG
              END-IF

              *> 是否為 號
              IF TEMP-LEN - CNT(IDX) = 1 AND
                 TEMP-COL(TEMP-LEN:1) IS ALPHABETIC-UPPER
                MOVE "," TO PRE-FLAG
                MOVE "Y" TO TEMP-FLAG
              END-IF

              *> 本身便帶有","
              IF TEMP-COL(TEMP-LEN + 1:1) = ","
                MOVE "Y" TO TEMP-FLAG
              END-IF

       
           *> ===== (NEXT-COL)特定字判斷 =====
              IF TEMP-FLAG NOT = "Y"
              MOVE FUNCTION UPPER-CASE(NEXT-COL) TO NEXT-UPPER-COL
              IF NEXT-UPPER-COL(NEXT-LEN:1) = ","
                MOVE NEXT-UPPER-COL(1:NEXT-LEN - 1) TO NEXT-UPPER-COL
              END-IF

              *> 以CategoryRule.csv 為準則切割
              PERFORM VARYING JDX FROM 4 BY 1 UNTIL JDX > 16
              PERFORM VARYING KDX FROM 1 BY 1 UNTIL KDX > 40
                MOVE LS-LIST-COL(JDX KDX) TO CHECK-COL
                IF CHECK-COL = SPACES OR CHECK-COL = ALL LOW-VALUES
                  EXIT PERFORM
                END-IF
                IF NEXT-UPPER-COL = CHECK-COL
                  MOVE "Y" TO TEMP-FLAG
                  MOVE "Y" TO NEXT-FLAG

                  MOVE FUNCTION UPPER-CASE(TEMP-PART(IDX + 2))
                    TO NEXT-UPPER-COL
                  IF NEXT-UPPER-COL(LENGTH OF 
                    FUNCTION TRIM(NEXT-UPPER-COL):1) = ","
                      MOVE NEXT-UPPER-COL(1:LENGTH OF 
                    FUNCTION TRIM(NEXT-UPPER-COL) - 1) TO NEXT-UPPER-COL
                  END-IF
                    *> 全方向
                    IF NEXT-UPPER-COL = "NORTH" OR
                       NEXT-UPPER-COL = "SOUTH" OR
                       NEXT-UPPER-COL = "EAST"  OR
                       NEXT-UPPER-COL = "WEST"  OR
                       NEXT-UPPER-COL = "WEST"  OR
                       NEXT-UPPER-COL = "NE"    OR
                       NEXT-UPPER-COL = "NW"    OR
                       NEXT-UPPER-COL = "SE"    OR
                       NEXT-UPPER-COL = "SW"    OR
                       NEXT-UPPER-COL = "N"     OR
                       NEXT-UPPER-COL = "S"     OR
                       NEXT-UPPER-COL = "E"     OR
                       NEXT-UPPER-COL = "W"

                       STRING
                          FUNCTION TRIM(TEMP-PART(IDX + 1))
                            DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(TEMP-PART(IDX + 2))
                            DELIMITED BY SIZE
                          INTO NEXT-COL
                       END-STRING

                       ADD 1 TO IDX

                    END-IF
                  EXIT PERFORM
                END-IF
              END-PERFORM
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
                   ADD 1 TO IDX
              END-IF

           *> PRE-FLAG
              IF PRE-COL(LENGTH OF FUNCTION TRIM(PRE-COL):1) = "," OR
                 IDX = 1 OR PRE-LEN = 0
                MOVE SPACES TO PRE-FLAG
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

              IF TEMP-FLAG = "Y"
                IF TEMP-COL(TEMP-LEN + 1:1) = ","
                  STRING
                    FUNCTION TRIM(AFTER-DATA) DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
                    INTO AFTER-DATA
                  END-STRING
                ELSE
                  STRING
                    FUNCTION TRIM(AFTER-DATA) DELIMITED BY SIZE
                    " " DELIMITED BY SIZE
                    FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
                    "," DELIMITED BY SIZE
                    INTO AFTER-DATA
                  END-STRING
                END-IF
           END-IF

      *     *> ","插入判斷結果
      *    DISPLAY "IDX : "IDX "/ " TEMP-FLAG
      *    DISPLAY "CNT : "CNT(IDX)"/ "CNT(IDX + 1)
      *    DISPLAY "PRE : "FUNCTION TRIM(PRE-COL)"/ "PRE-LEN"/ "PRE-FLAG
      *    DISPLAY "TEMP: "
      *            FUNCTION TRIM(TEMP-COL) "/ " TEMP-LEN"/ "CNT(IDX)
      *    DISPLAY "MIX : "NEXT-FLAG
      *    DISPLAY "NEXT: "
      *            FUNCTION TRIM(NEXT-COL)"/ "NEXT-LEN"/ "CNT(IDX + 1)
      *    DISPLAY FUNCTION TRIM(AFTER-DATA)
      *    DISPLAY "------------- ------------- -------------"


           END-PERFORM.

       *>  ====================== 最終調整 ======================
           *> 結尾
           IF TEMP-FLAG = "N"

             IF AFTER-DATA(LENGTH OF FUNCTION TRIM(AFTER-DATA):1) = ","
               MOVE SPACES TO PRE-FLAG
             ELSE
               MOVE ", " TO PRE-FLAG
             END-IF

             STRING
               FUNCTION TRIM(PRE-FLAG) DELIMITED BY SIZE
               FUNCTION TRIM(AFTER-DATA) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(PRE-COL) DELIMITED BY SIZE
               INTO AFTER-DATA
             END-STRING
           END-IF.

           IF AFTER-DATA(LENGTH OF FUNCTION TRIM(AFTER-DATA):1) = ","
             MOVE AFTER-DATA(1:LENGTH OF FUNCTION TRIM(AFTER-DATA) - 1)
               TO AFTER-DATA
           END-IF.

           *> ,, 置換
           INSPECT AFTER-DATA REPLACING ALL ",," BY ", ".

           *> 開頭,
           MOVE LENGTH OF FUNCTION TRIM(AFTER-DATA) TO TEMP-LEN.
           IF AFTER-DATA(1:1) = ","
             MOVE FUNCTION TRIM(AFTER-DATA(2:TEMP-LEN))
               TO AFTER-DATA
           END-IF.

           *> 結尾,
           MOVE LENGTH OF FUNCTION TRIM(AFTER-DATA) TO TEMP-LEN.
           IF AFTER-DATA(TEMP-LEN:1) = ","
             MOVE FUNCTION TRIM(AFTER-DATA(1:TEMP-LEN - 1))
               TO AFTER-DATA
           END-IF.

           *> 處理結束
           DISPLAY "MOTO : "FUNCTION TRIM(BEFORE-DATA).
           DISPLAY "KEKKA: "FUNCTION TRIM(AFTER-DATA).

           *> 處理結束
           EXIT PROGRAM.
       END PROGRAM FORMATTER-ADDRESS.
