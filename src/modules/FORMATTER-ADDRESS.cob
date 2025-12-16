       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMATTER-ADDRESS.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *******************************************************
      *> 資料部、LOCAL-STORAGE SECTION
      *******************************************************
       LOCAL-STORAGE SECTION.

       01  WK-AREA.
           03  IDX                       PIC   9(03).
           03  JDX                       PIC   9(03).
           03  KDX                       PIC   9(02).
           03  WK-DURING-PROCESS.
             05  WK-PART                 PIC   X(500) OCCURS 30 TIMES.
             05  WK-TEMP-COL             PIC   X(500).
             05  WK-TEMP-LEN             PIC   9(03).
             05  WK-CNT0                 PIC   9(03).
             05  WK-CNT1                 PIC   9(03).
             05  WK-TEMP-FLAG            PIC   XX VALUE "Y".
               88  TEMP-FLAG-Y                    VALUE "Y".
               88  TEMP-FLAG-N                    VALUE "N".

           03  WK-RULE-RTN.
             05  RR-TEMP.
               07  RR-TEMP-COL           PIC   X(500).
               07  RR-TEMP-LEN           PIC   9(03).
               07  RR-TEMP-FLAG          PIC   XX VALUE "Y".
                 *> Y: 需要分類
                 88  RR-TEMP-Y                    VALUE "Y".
                 88  RR-TEMP-N                    VALUE "N".

             05  RR-NEXT.
               07  RR-NEXT-COL           PIC   X(500).
               07  RR-NEXT-LEN           PIC   9(03).
               07  RR-NEXT-FLAG          PIC   XX VALUE "Y".
                 *> Y: 下一欄需要和TEMP一起分類
                 88  RR-NEXT-Y                    VALUE "Y".
                 88  RR-NEXT-N                    VALUE "N".

             05  RR-PRE.
               07  RR-PRE-COL            PIC   X(500).
               07  RR-PRE-LEN            PIC   9(03).
               07  RR-PRE-FLAG           PIC   XX VALUE "Y".
                 *> Y: PRE內容不可和TEMP混合(需切開)
                 88  RR-PRE-Y                     VALUE ",".

             05  RR-IDX-PLUS             PIC   9(02).
             05  RR-DTLS-FLAG            PIC   9(02).

             05  RR-FLOOR.
               07  RR-CNT-F              PIC   9(02).
               07  RR-CNT-FL             PIC   9.

             05  RR-TEMP-AB.
               07  RR-TEMP-A             PIC   X(500).
               07  RR-TEMP-A-LEN         PIC   9(03).
               07  RR-TEMP-B             PIC   X(500).
               07  RR-TEMP-B-LEN         PIC   9(03).
             
             05  RR-CHECK.
               07  RR-CHECK-COL          PIC   X(100).
               07  RR-CHECK-LEN          PIC   9(03).
             05 RR-FOUND-JDX             PIC   9(3).
             05 RR-FOUND-JDX-LEN         PIC   9(2).

           *> 記錄處理中狀態
           03 WK-PROCESS-CHECK.
             *> WK-PART 該IDX歸類位置
             05 PC-PART-CHECK            PIC   99    OCCURS 30 TIMES.
             05 PC-MATCH-NEW             PIC   99.
             *> 數字字數 (包含  "-")
             05 PC-CNT-NUM               PIC   9(3)  OCCURS 30 TIMES.
             *> 文字字數 (不包含 ","/數字)
             05 PC-CNT-CHAR              PIC   9(3)  OCCURS 30 TIMES.
             *> 是否為 "," 結尾
             05 PC-CNT-COMMA             PIC   X     OCCURS 30 TIMES.
             *> 是否為 關鍵字 (99: 表示方向)
             05 PC-CNT-KEY-I             PIC   9(2)  OCCURS 30 TIMES. *> INDEX
             05 PC-CNT-KEY-W             PIC   X(35) OCCURS 30 TIMES. *> WORD

             *> REBUILD
             05 RB-DTLS-LF               PIC  9(3) OCCURS 30 TIMES.

             05 PC-OTHER-FLAG            PIC   X.
             05 PC-OTHER-CITY            PIC   99. *> 3
             05 PC-OTHER-DISTRICT        PIC   99. *> 4
             05 PC-OTHER-STREET          PIC   99. *> 5
             05 PC-OTHER-PROVINCE        PIC   99. *> 16
             05 PC-OTHER-STATE           PIC   99. *> 17
             05 PC-OTHER-PRE             PIC   99. *> 前次記錄位置
             05 PC-OTHER-NEXT            PIC   99. *> 下次記錄位置

           *> FA-TRIM
           03 TRIM-DATA.
             05  TD-TEMP                 PIC   X(500). *> IN
             05  TD-FINISH               PIC   X(500). *> OUT
             05  TD-IDX                  PIC   9(03).
             05  TD-LEN                  PIC   9(03).
             05  TD-S                    PIC   9(03).  *> START
             05  TD-L                    PIC   9(03).  *> LEN

           *> FA-MERGE-RTN
           03 MERGE-DATA.
             05  MD-TEMP.
               07  MD-STRING-A           PIC   X(200).
               07  MD-LEN-A              PIC   9(03).
               07  MD-STRING-B           PIC   X(200).
               07  MD-LEN-B              PIC   9(03).
               07  MD-FLAG               PIC   X.
                 *> N: 不需合併
                 88  MD-FLAG-N                    VALUE "N".
               *> A B之間是否插入空白之外的文字 "A B" -> "A, B"
               07  MD-COL                PIC   X(1).
             05  MD-FINISH               PIC   X(200).
             05  MD-L                    PIC   9(03).

       *> ERROR 訊息判斷
           03 ERROR-ARRAY.
             05 COMMA-FLAG               PIC   XX.
             05 WS-CH                    PIC   X.
             05 WS-CODE                  PIC   9(5).
             05 ALLOWED-CH               PIC   X(20) VALUE "/-?:().,+'".

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
              10  LS-LIST-COL       PIC X(35) OCCURS 50 TIMES.
           05  LS-COUNTRY-NAME      PIC X(50) OCCURS 500 TIMES.
           05  LS-COUNTRY-CODE      PIC X(2)  OCCURS 500 TIMES.
           05  LS-STATE-NAME        PIC X(45) OCCURS 250 TIMES.
           05  LS-STATE-CODE        PIC X(10) OCCURS 250 TIMES.
           05  LS-STATE-COUNTRY     PIC X(2)  OCCURS 250 TIMES.
           05  DIR-NAMES            OCCURS 23 TIMES PIC X(8). *> 全方向
           05  DIR-LEN              PIC 99    VALUE 23.
           05  EXCEPTION-WORD-TABLE.
              10  EXCEPTION-WORD    OCCURS 10 TIMES PIC X(20).
              10  EXCEPTION-FLAG    OCCURS 10 TIMES PIC 9(2).
              10  EXCEPTION-COUNTRY OCCURS 10 TIMES PIC X(2).
              10  EXCEPTION-LEN     PIC 99    VALUE 10.

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-LIST-REC LS-FORMATTER.

      *******************************************************
      *> INITIALIZATION SECTION 初始化
      *******************************************************
       INITIALIZATION SECTION.
           INITIALIZE WK-AREA.

      *******************************************************
      *> MAIN SECTION 執行/檢查
      *******************************************************
       MAIN SECTION.
           PERFORM FA-DATA-FORMATTING. *> 1. 整頓原始資料
           INITIALIZE  WK-DURING-PROCESS WK-PROCESS-CHECK.
      *    DISPLAY "FORMAT        :"FUNCTION TRIM(BEFORE-DATA).
           PERFORM FA-EXTRACT-COUNTRY. *> 2. 抽出 國家
      *    DISPLAY "CLEAR COUNTRY :"FUNCTION TRIM(BEFORE-DATA)
      *            "/ " FUNCTION TRIM(DTLS-LF(2)).
           PERFORM FA-EXTRACT-STATE.   *> 3. 抽出 STATE
      *    DISPLAY "CLEAR STATE   :"FUNCTION TRIM(BEFORE-DATA)
      *            "/ " FUNCTION TRIM(DTLS-LF(17)).
           PERFORM FA-EXTRACT-EW.      *> 4. 抽出 EXCEPTION-WORD
           PERFORM FA-COUNT-CNT.       *> 5. 計算各欄位字數 數字判斷
           PERFORM FA-SPLIT-BY-LOGIC.  *> 6. 依照 分割規則 拆解地址
           PERFORM FA-REBUILD.         *> 7. 反結構
           PERFORM FA-ERROR-SECTION.   *> 8. 處理錯誤資料

      *    DISPLAY "=========== KEKKA."
      *    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
      *    EVALUATE IDX
      *      WHEN 1
      *        MOVE "ZIP      :" TO RR-TEMP-COL
      *      WHEN 2
      *        MOVE "COUNTRY  :" TO RR-TEMP-COL
      *      WHEN 3
      *        MOVE "CITY     :" TO RR-TEMP-COL
      *      WHEN 4
      *        MOVE "DIST.    :" TO RR-TEMP-COL
      *      WHEN 5
      *        MOVE "STREET   :" TO RR-TEMP-COL
      *      WHEN 6
      *        MOVE "SEC.     :" TO RR-TEMP-COL
      *      WHEN 7
      *        MOVE "LANE     :" TO RR-TEMP-COL
      *      WHEN 8
      *        MOVE "ALLEY    :" TO RR-TEMP-COL
      *      WHEN 9
      *        MOVE "NO.      :" TO RR-TEMP-COL
      *      WHEN 10
      *        MOVE "DEPT.    :" TO RR-TEMP-COL
      *      WHEN 11
      *        MOVE "FLOOR    :" TO RR-TEMP-COL
      *      WHEN 12
      *        MOVE "P.O.-BOX :" TO RR-TEMP-COL
      *      WHEN 13
      *        MOVE "ROOM     :" TO RR-TEMP-COL
      *      WHEN 14
      *        MOVE "BUILDING :" TO RR-TEMP-COL
      *      WHEN 15
      *        MOVE "VILLAGE  :" TO RR-TEMP-COL
      *      WHEN 16
      *        MOVE "PROVINCE :" TO RR-TEMP-COL
      *      WHEN 17
      *        MOVE "STATE    :" TO RR-TEMP-COL
      *      WHEN 18
      *        MOVE "SUB DEPT.:" TO RR-TEMP-COL
      *    END-EVALUATE
      *
      *    DISPLAY FUNCTION TRIM(RR-TEMP-COL)"/ "
      *            FUNCTION TRIM(DTLS-LF(IDX))
      *    END-PERFORM.

           GOBACK.

      *******************************************************
      *> FA-DATA-FORMATTING SECTION 整頓原始資料
      *******************************************************
       FA-DATA-FORMATTING.
           *> 特殊狀況處理
           *> 字首 "ADD." -> SPACES
           IF BEFORE-DATA(1:4) = "ADD."
             MOVE BEFORE-DATA(5:TD-L - 4) TO BEFORE-DATA
           END-IF.

           MOVE      BEFORE-DATA       TO    TD-TEMP.
           PERFORM   FA-TRIM.
           MOVE      TD-FINISH           TO    BEFORE-DATA.

           *> 字首 "ON " -> SPACES
           IF BEFORE-DATA(1:3) = "ON "
             MOVE BEFORE-DATA(4:TD-L - 3) TO BEFORE-DATA
           END-IF.

           *> : -> .
           INSPECT   BEFORE-DATA        REPLACING ALL ":"
                                                   BY ".".

           MOVE      BEFORE-DATA       TO    TD-TEMP.
           PERFORM   FA-TRIM.
           MOVE      TD-FINISH           TO    BEFORE-DATA.

           IF BEFORE-DATA(TD-L:1) = "."
             MOVE BEFORE-DATA(1: TD-L - 1) TO BEFORE-DATA
           END-IF.

           *> C.A.P. -> SPACES (ZIP)
           INSPECT BEFORE-DATA REPLACING FIRST "C.A.P." BY "      ".

           *> "CITYXXX" -> "CITY"
      *    PERFORM VARYING IDX FROM TD-L BY -1 UNTIL IDX = 4
      *      IF BEFORE-DATA(IDX - 4:4) = "CITY" AND
      *         BEFORE-DATA(IDX:1) NOT = " "
      *        MOVE BEFORE-DATA(IDX:TD-L - IDX + 1)
      *             TO RR-TEMP-A(IDX + 1:TD-L - IDX + 1)
      *        MOVE RR-TEMP-A(IDX:TD-L - IDX + 2)
      *             TO BEFORE-DATA(IDX:TD-L - IDX + 2)
      *      END-IF
      *    END-PERFORM.


           *> 整頓標點符號/特殊字前後" "&","配置
           *> "," -> ", "
           MOVE      TD-L                TO    RR-TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > RR-TEMP-LEN
               *> 9 - 9 -> 9-9
               IF IDX > 4 AND
                  BEFORE-DATA(IDX:1) IS NUMERIC AND
                  BEFORE-DATA(IDX + 1:1) = SPACES AND
                  BEFORE-DATA(IDX + 2:1) = "-" AND
                  BEFORE-DATA(IDX + 3:1) = SPACES AND
                  BEFORE-DATA(IDX + 4:1) IS NUMERIC
                 MOVE SPACES TO WK-TEMP-COL

                 MOVE BEFORE-DATA(1:IDX) TO WK-TEMP-COL
                 MOVE "-" TO WK-TEMP-COL(IDX + 1:1)
                 MOVE BEFORE-DATA(IDX + 4:RR-TEMP-LEN - IDX - 3)
                      TO WK-TEMP-COL(IDX + 2:RR-TEMP-LEN - IDX - 3)
                 ADD 1 TO IDX
                 MOVE WK-TEMP-COL TO BEFORE-DATA

                 MOVE      BEFORE-DATA       TO    TD-TEMP
                 PERFORM   FA-TRIM
                 MOVE      TD-L                TO    RR-TEMP-LEN
               END-IF

               *> 移除字首 ","
               IF BEFORE-DATA(IDX:1) = "," AND IDX = 1
                 SUBTRACT 1 FROM RR-TEMP-LEN IDX
                 MOVE      BEFORE-DATA(2:RR-TEMP-LEN)     TO    TD-TEMP
                 PERFORM   FA-TRIM
                 MOVE      TD-FINISH           TO    BEFORE-DATA
                 MOVE      TD-L                TO    RR-TEMP-LEN
               END-IF

               *> 移除字尾 ","
               IF BEFORE-DATA(IDX:1) = "," AND IDX = RR-TEMP-LEN
                 SUBTRACT 2 FROM IDX

                 MOVE      BEFORE-DATA(1:RR-TEMP-LEN - 1)  TO    TD-TEMP
                 PERFORM   FA-TRIM
                 MOVE      TD-FINISH           TO    BEFORE-DATA
                 MOVE      TD-L                TO    RR-TEMP-LEN
               END-IF

               *> 移除重複 ","
               IF BEFORE-DATA(IDX:2) = ",,"
                 MOVE SPACES TO WK-TEMP-COL

                 MOVE BEFORE-DATA(1:IDX) TO WK-TEMP-COL
                 MOVE BEFORE-DATA(IDX + 2:RR-TEMP-LEN - IDX - 1)
                      TO WK-TEMP-COL(IDX + 1:RR-TEMP-LEN - IDX - 1)
                 SUBTRACT 1 FROM IDX
                 MOVE WK-TEMP-COL TO BEFORE-DATA

                 MOVE      BEFORE-DATA       TO    TD-TEMP
                 PERFORM   FA-TRIM
                 MOVE      TD-L                TO    RR-TEMP-LEN
               END-IF

               *> ",X" -> ", X" OR ".,X" -> "., X" OR "X.9" -> "X. 9"
               IF (BEFORE-DATA(IDX:1) = ","
                  AND BEFORE-DATA(IDX + 1:1) NOT = " ") 
                  OR
                  (IDX > 1 AND
                    BEFORE-DATA(IDX - 1:1) IS ALPHABETIC AND
                  BEFORE-DATA(IDX:1) = "." AND
                  BEFORE-DATA(IDX + 1:1) IS NUMERIC)
                    MOVE IDX TO MD-LEN-A
                    MOVE BEFORE-DATA(1:MD-LEN-A) TO MD-STRING-A
                    COMPUTE MD-LEN-B = RR-TEMP-LEN - IDX
                    MOVE BEFORE-DATA(IDX + 1:MD-LEN-B) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE      MD-L             TO    RR-TEMP-LEN
                    MOVE      MD-FINISH        TO    BEFORE-DATA
               END-IF
           END-PERFORM.

           MOVE      FUNCTION UPPER-CASE(BEFORE-DATA)
                                         TO    BEFORE-DATA.
           *> 迴避複數空格的狀況、縮寫地址插入"."
           UNSTRING
             BEFORE-DATA DELIMITED BY ALL " "
             INTO WK-PART (1)
               WK-PART (2)
               WK-PART (3)
               WK-PART (4)
               WK-PART (5)
               WK-PART (6)
               WK-PART (7)
               WK-PART (8)
               WK-PART (9)
               WK-PART (10)
               WK-PART (11)
               WK-PART (12)
               WK-PART (13)
               WK-PART (14)
               WK-PART (15)
               WK-PART (16)
               WK-PART (17)
               WK-PART (18)
               WK-PART (19)
               WK-PART (20)
               WK-PART (21)
               WK-PART (22)
               WK-PART (23)
               WK-PART (24)
               WK-PART (25)
               WK-PART (26)
               WK-PART (27)
               WK-PART (28)
               WK-PART (29)
               WK-PART (30)
           .

           MOVE SPACES TO BEFORE-DATA.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF WK-PART(IDX) NOT = SPACES
               *> 判斷前字串字尾是否為 "," 是則跳過+.判斷
               IF IDX > 1 AND WK-TEMP-COL(RR-TEMP-LEN:1) = ","
                 MOVE "Y" TO RR-NEXT-FLAG
               ELSE
                 MOVE "N" TO RR-NEXT-FLAG
               END-IF

               MOVE      WK-PART(IDX)        TO    TD-TEMP
               PERFORM   FA-TRIM
               MOVE      TD-FINISH           TO    WK-TEMP-COL
               MOVE      TD-L                TO    RR-TEMP-LEN
               IF WK-TEMP-COL(RR-TEMP-LEN:1) = ","
                 MOVE WK-TEMP-COL(1:RR-TEMP-LEN - 1) TO WK-TEMP-COL
               END-IF

               MOVE "N" TO WK-TEMP-FLAG
               MOVE      WK-PART(IDX)        TO    TD-TEMP
               PERFORM   FA-TRIM
               MOVE      TD-L                TO    RR-TEMP-LEN
              IF WK-PART(IDX)(RR-TEMP-LEN:1) = ","
                MOVE 1 TO WK-CNT1
              ELSE
                MOVE 0 TO WK-CNT1
              END-IF

              *> 2文字
              MOVE WK-PART(IDX)(1:2) TO WK-TEMP-COL
              IF ((WK-TEMP-COL = "ST" AND RR-NEXT-N) OR
                  (WK-TEMP-COL = "RD" AND RR-NEXT-N) OR
                  WK-TEMP-COL = "DR" OR
      *           WK-TEMP-COL = "RM" OR
                  WK-TEMP-COL = "NO" OR
                  WK-TEMP-COL = "LN") AND
                  WK-PART(IDX)(3:1) NOT = "." AND RR-TEMP-LEN < 4
                 MOVE "Y" TO WK-TEMP-FLAG
              END-IF

              *> 3文字
              MOVE WK-PART(IDX)(1:3) TO WK-TEMP-COL
              IF (WK-TEMP-COL = "AVE" OR
                  WK-TEMP-COL = "RIV" OR
                  WK-TEMP-COL = "ALY" OR
                  WK-TEMP-COL = "LTD" OR
                  WK-TEMP-COL = "P.O") AND
                  WK-PART(IDX)(4:1) NOT = "." AND RR-TEMP-LEN < 5
                 MOVE "Y" TO WK-TEMP-FLAG
              END-IF

              *> 4文字
              MOVE WK-PART(IDX)(1:4) TO WK-TEMP-COL
              IF (WK-TEMP-COL = "BLDG" OR
                  WK-TEMP-COL = "BLVD" OR
                  WK-TEMP-COL = "DIST" OR
                  WK-TEMP-COL = "DEPT") AND
                  WK-PART(IDX)(5:1) NOT = "." AND RR-TEMP-LEN < 6
                 MOVE "Y" TO WK-TEMP-FLAG
              END-IF

              *> 補上.
              IF TEMP-FLAG-Y
                 MOVE WK-PART(IDX)(1:RR-TEMP-LEN - WK-CNT1)
                                                          TO WK-TEMP-COL
                 MOVE "."  TO   WK-TEMP-COL(RR-TEMP-LEN - WK-CNT1 + 1:1)
                 MOVE WK-PART(IDX)(RR-TEMP-LEN - WK-CNT1 + 1:WK-CNT1) 
                      TO  WK-TEMP-COL(RR-TEMP-LEN - WK-CNT1 + 2:WK-CNT1)

                 MOVE WK-TEMP-COL TO WK-PART(IDX)
              END-IF

              IF WK-PART(IDX)(3:1) = "." AND
                 WK-PART(IDX)(4:1) IS NUMERIC
                   MOVE      WK-PART(IDX) TO WK-TEMP-COL
                   MOVE      WK-TEMP-COL       TO    TD-TEMP
                   PERFORM   FA-TRIM
                   MOVE      TD-L              TO    RR-TEMP-LEN

                   MOVE      3                 TO    MD-LEN-A
                   MOVE      WK-PART(IDX)(1:3) TO    MD-STRING-A
                   COMPUTE   MD-LEN-B = RR-TEMP-LEN - 3
                   MOVE      WK-PART(IDX)(4:MD-LEN-B) TO MD-STRING-B
                   PERFORM   FA-MERGE-RTN
                   MOVE      MD-FINISH         TO    WK-TEMP-COL

                   MOVE WK-TEMP-COL TO WK-PART(IDX)
              END-IF

               MOVE      BEFORE-DATA         TO    MD-STRING-A
               MOVE      WK-PART(IDX)        TO    MD-STRING-B
               PERFORM   FA-MERGE-RTN
               MOVE      MD-FINISH           TO    BEFORE-DATA
             END-IF
           END-PERFORM.


           *> POSTAL CODE -> SPACES
           INSPECT   BEFORE-DATA        REPLACING ALL "POSTAL CODE"
                                                   BY "           ".
           *> P.O. Box -> P.O.-BOX
           INSPECT   BEFORE-DATA        REPLACING ALL "P.O. BOX"
                                                   BY "P.O.-BOX".
           *> PO Box -> PO-Box
           INSPECT   BEFORE-DATA        REPLACING ALL "PO BOX"
                                                   BY "PO-BOX".
           *> PRIVATE BAG -> PRIVATE-BAG
           INSPECT   BEFORE-DATA        REPLACING ALL "PRIVATE BAG"
                                                   BY "PRIVATE-BAG".
           *> P.C. -> SPACES
           INSPECT   BEFORE-DATA        REPLACING ALL "P.C."
                                                   BY "    ".
           *> " - " -> ",  "
           INSPECT   BEFORE-DATA        REPLACING ALL " - "
                                                   BY ",  ".

      *******************************************************
      *> FA-EXTRACT-COUNTRY SECTION 抽出 國家
      *******************************************************
       FA-EXTRACT-COUNTRY.
          *> COUNTRY抽出作業
           MOVE BEFORE-DATA TO TD-TEMP.
           PERFORM FA-TRIM.
           MOVE TD-L TO RR-TEMP-LEN.
           MOVE 0 TO RR-FOUND-JDX RR-FOUND-JDX-LEN.
           *> RR-TEMP-COL -> 原字串
           *> RR-TEMP-A   -> 資料字串(國家全名)
           *> RR-CNT-F    -> , 出現次數
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 500
             IF LS-COUNTRY-NAME(IDX) = SPACES
               EXIT PERFORM
             END-IF

             MOVE LS-COUNTRY-NAME(IDX) TO RR-TEMP-A TD-TEMP
             PERFORM FA-TRIM
             MOVE TD-L TO RR-TEMP-A-LEN

             MOVE 0 TO RR-CNT-F *> 紀錄逗號
             PERFORM VARYING JDX FROM RR-TEMP-LEN BY -1
                     UNTIL JDX = RR-FOUND-JDX

               IF BEFORE-DATA(JDX:1) = " " AND
                  RR-TEMP-LEN - JDX > RR-TEMP-A-LEN
                    ADD 1 TO RR-CNT-F
                    IF RR-CNT-F > 2
                      EXIT PERFORM
                    END-IF
               END-IF

               *> 若文字串中包含 國家全名
               IF BEFORE-DATA(JDX:RR-TEMP-A-LEN) = RR-TEMP-A AND
                  JDX >=  RR-FOUND-JDX + RR-FOUND-JDX-LEN AND
                  LS-COUNTRY-CODE(IDX) NOT = DTLS-LF(2) AND
                  (JDX = 1 OR
                   BEFORE-DATA(JDX - 1:1) = SPACES OR
                   BEFORE-DATA(JDX - 1:1) = ",") AND
                  (BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = SPACE OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = "," OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = ".")
                  MOVE LS-COUNTRY-CODE(IDX) TO DTLS-LF(2)
                  MOVE JDX TO RR-FOUND-JDX
                  MOVE RR-TEMP-A-LEN TO RR-FOUND-JDX-LEN

                  *> 後半字串起始點 -> RR-TEMP-B-LEN
                  COMPUTE RR-TEMP-B-LEN = JDX + RR-TEMP-A-LEN
                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  *> 後半字串長度 -> RR-TEMP-A-LEN
                  IF RR-TEMP-LEN - RR-TEMP-B-LEN + 1 <= 0
                    MOVE 0 TO RR-TEMP-A-LEN
                  ELSE
                    COMPUTE RR-TEMP-A-LEN 
                            = RR-TEMP-LEN - RR-TEMP-B-LEN + 1
                  END-IF

                  MOVE SPACES TO RR-TEMP-COL
                  MOVE BEFORE-DATA(1:JDX - 1) TO RR-TEMP-COL(1:JDX - 1)
                  MOVE BEFORE-DATA(RR-TEMP-B-LEN:RR-TEMP-A-LEN)
                       TO RR-TEMP-COL(JDX:RR-TEMP-A-LEN)
                  EXIT PERFORM
               END-IF

               *> 若文字串中包含 國家ISO代號
               IF BEFORE-DATA(JDX:2) = LS-COUNTRY-CODE(IDX) AND
                  JDX >= RR-FOUND-JDX AND
                  JDX >  RR-FOUND-JDX + RR-FOUND-JDX-LEN AND
                  LS-COUNTRY-CODE(IDX) NOT = DTLS-LF(2) AND
                  (BEFORE-DATA(JDX - 1:1) = SPACES OR
                   BEFORE-DATA(JDX - 1:1) = ",") AND
                  (BEFORE-DATA(JDX + 2:1) = SPACE OR
                   BEFORE-DATA(JDX + 2:1) = ",")
                  MOVE LS-COUNTRY-CODE(IDX) TO DTLS-LF(2)
                  MOVE JDX TO RR-FOUND-JDX
                  MOVE 2 TO RR-FOUND-JDX-LEN

                  *> 後半字串起始點 -> RR-TEMP-B-LEN
                  COMPUTE RR-TEMP-B-LEN = JDX + 2
                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = ","
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = ","
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  *> 後半字串長度 -> RR-TEMP-A-LEN
                  IF RR-TEMP-LEN - RR-TEMP-B-LEN + 1 <= 0
                    MOVE 0 TO RR-TEMP-A-LEN
                  ELSE
                    COMPUTE RR-TEMP-A-LEN 
                            = RR-TEMP-LEN - RR-TEMP-B-LEN + 1
                  END-IF

                  MOVE SPACES TO RR-TEMP-COL
                  MOVE BEFORE-DATA(1:JDX - 1) TO RR-TEMP-COL(1:JDX - 1)
                  MOVE BEFORE-DATA(RR-TEMP-B-LEN:RR-TEMP-A-LEN)
                       TO RR-TEMP-COL(JDX:RR-TEMP-A-LEN)
                  EXIT PERFORM
               END-IF

             END-PERFORM
           END-PERFORM.

           IF DTLS-LF(2) NOT = SPACES
             MOVE RR-TEMP-COL TO TD-TEMP
             PERFORM FA-TRIM
             IF TD-TEMP(TD-L:1) = ","
               MOVE TD-TEMP(1:TD-L - 1) TO TD-TEMP
               PERFORM FA-TRIM
             END-IF

             MOVE TD-TEMP TO BEFORE-DATA
           END-IF.

      *******************************************************
      *> FA-EXTRACT-STATE SECTION 抽出 STATE
      *******************************************************
       FA-EXTRACT-STATE.

           *> 若判斷對象非以下國家 跳過STATE抽出作業
           IF NOT 
             (DTLS-LF(2) = "US" OR
              DTLS-LF(2) = "AU" OR
              DTLS-LF(2) = "IN" OR
              DTLS-LF(2) = "MX" OR
              DTLS-LF(2) = "CA" OR
              DTLS-LF(2) = "IT" OR
              DTLS-LF(2) = "PW" OR
              DTLS-LF(2) = "VN")
             EXIT PARAGRAPH
           END-IF.

           *> STATE抽出作業
           MOVE BEFORE-DATA TO TD-TEMP.
           *> J&K -> JAK(視為簡寫)
           INSPECT WK-TEMP-COL REPLACING ALL "&" BY "A".
           PERFORM FA-TRIM.
           MOVE TD-L TO RR-TEMP-LEN.
           MOVE 0 TO RR-FOUND-JDX.
           *> RR-TEMP-COL -> 原字串
           *> RR-TEMP-A   -> 資料字串(STATE全名)
           *> RR-CNT-F    -> , 出現次數
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 500
             IF LS-STATE-NAME(IDX) = SPACES
               EXIT PERFORM
             END-IF

             MOVE LS-STATE-NAME(IDX) TO RR-TEMP-A TD-TEMP
             PERFORM FA-TRIM
             MOVE TD-L TO RR-TEMP-A-LEN

             MOVE 0 TO RR-CNT-F *> 紀錄逗號
             PERFORM VARYING JDX FROM RR-TEMP-LEN BY -1
                     UNTIL JDX = RR-FOUND-JDX
               IF LS-STATE-COUNTRY(IDX) NOT = DTLS-LF(2)
                 EXIT PERFORM
               END-IF

               IF BEFORE-DATA(JDX:1) = " " AND
                  RR-TEMP-LEN - JDX > RR-TEMP-A-LEN
                    ADD 1 TO RR-CNT-F
                    IF RR-CNT-F > 2
                      EXIT PERFORM
                    END-IF
               END-IF

               *> 若文字串中包含 STATE全名
               IF BEFORE-DATA(JDX:RR-TEMP-A-LEN) = RR-TEMP-A AND
                  (JDX = 1 OR
                   BEFORE-DATA(JDX - 1:1) = SPACES OR
                   BEFORE-DATA(JDX - 1:1) = ",") AND
                  (BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = SPACE OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = "," OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = ".")
                  MOVE LS-STATE-CODE(IDX) TO DTLS-LF(17)
                  MOVE JDX TO RR-FOUND-JDX

                  *> 後半字串起始點 -> RR-TEMP-B-LEN
                  COMPUTE RR-TEMP-B-LEN = JDX + RR-TEMP-A-LEN
                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  *> 後半字串長度 -> RR-TEMP-A-LEN
                  IF RR-TEMP-LEN - RR-TEMP-B-LEN + 1 <= 0
                    MOVE 0 TO RR-TEMP-A-LEN
                  ELSE
                    COMPUTE RR-TEMP-A-LEN 
                            = RR-TEMP-LEN - RR-TEMP-B-LEN + 1
                  END-IF

                  MOVE SPACES TO RR-TEMP-COL
                  MOVE BEFORE-DATA(1:JDX - 1) TO RR-TEMP-COL(1:JDX - 1)
                  MOVE BEFORE-DATA(RR-TEMP-B-LEN:RR-TEMP-A-LEN)
                       TO RR-TEMP-COL(JDX:RR-TEMP-A-LEN)
                  EXIT PERFORM
               END-IF

               IF LS-STATE-CODE(IDX)(3:1) NOT = SPACES
                 MOVE 3 TO KDX
               ELSE
                 MOVE 2 TO KDX
               END-IF
               *> 若文字串中包含 STATE代號(KDX碼)
               IF BEFORE-DATA(JDX:KDX) = LS-STATE-CODE(IDX) AND
                  (BEFORE-DATA(JDX - 1:1) = SPACES OR
                   BEFORE-DATA(JDX - 1:1) = ",") AND
                  (BEFORE-DATA(JDX + KDX:1) = SPACE OR
                   BEFORE-DATA(JDX + KDX:1) = ",")
                  MOVE LS-STATE-CODE(IDX) TO DTLS-LF(17)
                  MOVE JDX TO RR-FOUND-JDX

                  *> 後半字串起始點 -> RR-TEMP-B-LEN
                  COMPUTE RR-TEMP-B-LEN = JDX + KDX
                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = ","
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = ","
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  *> 後半字串長度 -> RR-TEMP-A-LEN
                  IF RR-TEMP-LEN - RR-TEMP-B-LEN + 1 <= 0
                    MOVE 0 TO RR-TEMP-A-LEN
                  ELSE
                    COMPUTE RR-TEMP-A-LEN 
                            = RR-TEMP-LEN - RR-TEMP-B-LEN + 1
                  END-IF

                  MOVE SPACES TO RR-TEMP-COL
                  MOVE BEFORE-DATA(1:JDX - 1) TO RR-TEMP-COL(1:JDX - 1)
                  MOVE BEFORE-DATA(RR-TEMP-B-LEN:RR-TEMP-A-LEN)
                       TO RR-TEMP-COL(JDX:RR-TEMP-A-LEN)
                  EXIT PERFORM
               END-IF

             END-PERFORM
             IF DTLS-LF(17) NOT = SPACES
               EXIT PERFORM
             END-IF
           END-PERFORM.

           IF DTLS-LF(17) NOT = SPACES
             MOVE RR-TEMP-COL TO TD-TEMP
             PERFORM FA-TRIM
             IF TD-TEMP(TD-L:1) = ","
               MOVE TD-TEMP(1:TD-L - 1) TO TD-TEMP
               PERFORM FA-TRIM
             END-IF

             MOVE TD-TEMP TO BEFORE-DATA
           END-IF.

      *******************************************************
      *> FA-EXTRACT-EW SECTION 抽出 EXCEPTION-WORD
      *******************************************************
       FA-EXTRACT-EW.
           MOVE TD-L TO RR-TEMP-LEN.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > EXCEPTION-LEN
             MOVE EXCEPTION-WORD(IDX) TO RR-TEMP-A TD-TEMP
             PERFORM FA-TRIM
             MOVE TD-L TO RR-TEMP-A-LEN

             IF RR-TEMP-A = SPACES
               EXIT PERFORM
             END-IF

             IF EXCEPTION-COUNTRY(IDX) = DTLS-LF(2) OR
                EXCEPTION-COUNTRY(IDX) = SPACES
             PERFORM VARYING JDX FROM 1 BY 1
                     UNTIL JDX + RR-TEMP-A-LEN - 1 > RR-TEMP-LEN
               IF BEFORE-DATA(JDX:RR-TEMP-A-LEN) = RR-TEMP-A AND
                  (JDX = 1 OR
                   BEFORE-DATA(JDX - 1:1) = SPACES OR
                   BEFORE-DATA(JDX - 1:1) = ",") AND
                  (JDX + RR-TEMP-A-LEN = RR-TEMP-LEN OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = SPACES OR
                   BEFORE-DATA(JDX + RR-TEMP-A-LEN:1) = ",")
                     MOVE RR-TEMP-A TO DTLS-LF(EXCEPTION-FLAG(IDX))
                  MOVE JDX TO RR-FOUND-JDX

                  *> 後半字串起始點 -> RR-TEMP-B-LEN
                  COMPUTE RR-TEMP-B-LEN = JDX + RR-TEMP-A-LEN
                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  IF BEFORE-DATA(RR-TEMP-B-LEN:1) = SPACES OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "," OR
                     BEFORE-DATA(RR-TEMP-B-LEN:1) = "."
                     ADD 1 TO RR-TEMP-B-LEN
                  END-IF

                  *> 後半字串長度 -> RR-TEMP-A-LEN
                  IF RR-TEMP-LEN - RR-TEMP-B-LEN + 1 <= 0
                    MOVE 0 TO RR-TEMP-A-LEN
                  ELSE
                    COMPUTE RR-TEMP-A-LEN 
                            = RR-TEMP-LEN - RR-TEMP-B-LEN + 1
                  END-IF

                  MOVE SPACES TO RR-TEMP-COL
                  MOVE BEFORE-DATA(1:JDX - 1) TO RR-TEMP-COL(1:JDX - 1)
                  MOVE BEFORE-DATA(RR-TEMP-B-LEN:RR-TEMP-A-LEN)
                       TO RR-TEMP-COL(JDX:RR-TEMP-A-LEN)
                  MOVE RR-TEMP-COL TO BEFORE-DATA
                  EXIT PERFORM
               END-IF
             END-PERFORM
             END-IF

           END-PERFORM.

      *******************************************************
      *> FA-COUNT-CNT SECTION 計算各欄位字數 數字判斷
      *******************************************************
       FA-COUNT-CNT.
           MOVE      BEFORE-DATA       TO    TD-TEMP
           PERFORM   FA-TRIM

           UNSTRING
             BEFORE-DATA DELIMITED BY ALL " "
             INTO WK-PART (1)
                  WK-PART (2)
                  WK-PART (3)
                  WK-PART (4)
                  WK-PART (5)
                  WK-PART (6)
                  WK-PART (7)
                  WK-PART (8)
                  WK-PART (9)
                  WK-PART (10)
                  WK-PART (11)
                  WK-PART (12)
                  WK-PART (13)
                  WK-PART (14)
                  WK-PART (15)
                  WK-PART (16)
                  WK-PART (17)
                  WK-PART (18)
                  WK-PART (19)
                  WK-PART (20)
                  WK-PART (21)
                  WK-PART (22)
                  WK-PART (23)
                  WK-PART (24)
                  WK-PART (25)
                  WK-PART (26)
                  WK-PART (27)
                  WK-PART (28)
                  WK-PART (29)
                  WK-PART (30)
           .

           *> CNT計算 (數字判斷)
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF WK-PART(IDX) = SPACES
               MOVE    2                 TO    PC-PART-CHECK(IDX)
               IF DTLS-LF(17) NOT = SPACES
                 MOVE 99 TO PC-OTHER-STATE
               END-IF

               EXIT PERFORM
             END-IF

             *> CNT
             MOVE      WK-PART(IDX)      TO    TD-TEMP
             PERFORM   FA-TRIM
             MOVE      TD-FINISH         TO    WK-TEMP-COL
             MOVE      TD-L              TO    WK-TEMP-LEN
             PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > WK-TEMP-LEN
               IF WK-TEMP-COL(JDX:2) = ", "
                 MOVE  ","               TO    PC-CNT-COMMA(IDX)
                 MOVE  WK-TEMP-COL(1:JDX - 1) TO   WK-TEMP-COL
                 SUBTRACT 1 FROM WK-TEMP-LEN
                 EXIT PERFORM
               END-IF

               *> 數字判斷
               IF WK-TEMP-COL(JDX:1) IS NUMERIC OR
                  WK-TEMP-COL(JDX:1) = "-"
                 ADD   1                 TO    PC-CNT-NUM(IDX)
               ELSE
                 IF WK-TEMP-COL(JDX:1) NOT = ","
                   ADD   1                 TO    PC-CNT-CHAR(IDX)
                 END-IF
               END-IF
             END-PERFORM

              *> 以 LS-LIST-COL 為準則切割
             PERFORM VARYING JDX FROM 3 BY 1 UNTIL JDX > 18
             PERFORM VARYING KDX FROM 2 BY 1 UNTIL KDX > 50
               MOVE LS-LIST-COL(JDX KDX) TO RR-CHECK-COL
               MOVE LENGTH OF FUNCTION TRIM(RR-CHECK-COL)
                                         TO RR-CHECK-LEN
               IF RR-CHECK-COL = SPACES
                 EXIT PERFORM
               END-IF

                *> ===== 羅馬字 判斷 ====
                IF RR-CHECK-COL(1:1) = "-" AND 
                   WK-TEMP-COL(WK-TEMP-LEN - RR-CHECK-LEN + 1:
                               RR-CHECK-LEN) = RR-CHECK-COL
                    COMPUTE PC-CNT-KEY-I(IDX) = JDX + 30
                    MOVE RR-CHECK-COL TO PC-CNT-KEY-W(IDX)
                END-IF

                *> ===== 後接字 判斷 ====
                IF RR-CHECK-COL(RR-CHECK-LEN:1) = "-" AND 
                   WK-TEMP-COL = RR-CHECK-COL(1:RR-CHECK-LEN - 1)
                    COMPUTE PC-CNT-KEY-I(IDX) = JDX + 60
                    MOVE RR-CHECK-COL TO PC-CNT-KEY-W(IDX)
                END-IF

                IF PC-CNT-KEY-I(IDX) = 0 AND
                   WK-TEMP-COL = RR-CHECK-COL
                  MOVE JDX TO PC-CNT-KEY-I(IDX)
                  MOVE RR-CHECK-COL TO PC-CNT-KEY-W(IDX)
                END-IF
             END-PERFORM
             END-PERFORM

             *> 全方向
             IF PC-CNT-KEY-I(IDX) = 0
             PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > DIR-LEN
               IF WK-TEMP-COL = DIR-NAMES(JDX)
                 MOVE 99 TO PC-CNT-KEY-I(IDX)
      *          MOVE DIR-NAMES(JDX) TO PC-CNT-KEY-W(IDX)
                 EXIT PERFORM
               END-IF
             END-PERFORM
             END-IF
      *      DISPLAY IDX " PC-CNT-KEY: "PC-CNT-KEY-I(IDX)
      *              "/ "FUNCTION TRIM(PC-CNT-KEY-W(IDX))
      *              "/ "FUNCTION TRIM(WK-TEMP-COL)
           END-PERFORM.

      *******************************************************
      *> FA-SPLIT-BY-LOGIC SECTION 依照 分割規則 拆解地址
      *******************************************************
       FA-SPLIT-BY-LOGIC.
           INITIALIZE WK-RULE-RTN.

      ********************************************************
           *> 剩餘欄位分類
           *> RR-TEMP-COL: 本次迴圈處理
           *> RR-NEXT-COL: 下欄預約
           *>    RR-PRE-COL : 累積
       
           *> RR-TEMP-FLAG: 需重置PER/ 本次納入AFTER
           *> (PER-COL)~(RR-PRE-FLAG)~(RR-TEMP-COL)~(RR-TEMP-FLAG)~
           *> (RR-NEXT-COL)~(RR-NEXT-FLAG)
      *******************************************************
      *    MOVE      BEFORE-DATA       TO    TD-TEMP.
           MOVE      "N"                 TO    RR-TEMP-FLAG.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF WK-PART(IDX) = SPACES
               EXIT PERFORM
             END-IF

             *> TEMP
             COMPUTE   RR-TEMP-LEN =
                         PC-CNT-NUM(IDX) + PC-CNT-CHAR(IDX)
             MOVE      WK-PART(IDX)(1:RR-TEMP-LEN)   TO    RR-TEMP-COL
             MOVE      RR-TEMP-COL                   TO    WK-PART(IDX)

             *> RR-TEMP-Y(分類完成) -> 重製 RR-PRE
             IF RR-TEMP-Y OR RR-DTLS-FLAG NOT = 0 OR IDX = 1
               INITIALIZE  RR-PRE
               MOVE    "N"                           TO    RR-TEMP-FLAG
             ELSE
               IF IDX NOT = 1 AND RR-TEMP-N
                 MOVE 99                   TO    PC-PART-CHECK(IDX - 1)
                 MOVE SPACES               TO    WK-PART(IDX - 1)
                 MOVE RR-PRE-COL           TO    TD-TEMP
                 PERFORM  FA-TRIM
                 MOVE TD-FINISH            TO    RR-PRE-COL
                 MOVE TD-L                 TO    RR-PRE-LEN
               END-IF
             END-IF
             MOVE     0                    TO RR-DTLS-FLAG RR-IDX-PLUS

             *> NEXT
             IF IDX NOT = 30
               COMPUTE   RR-NEXT-LEN =
                         PC-CNT-NUM(IDX + 1) + PC-CNT-CHAR(IDX + 1)
               MOVE    WK-PART(IDX + 1)(1:RR-NEXT-LEN) TO   RR-NEXT-COL
               MOVE    "N"                             TO   RR-NEXT-FLAG
             END-IF

       *>  ====================== 判斷開始 ======================
             *> =================== NUMBER 判斷 ===================
             *> 是否為 單純數字
             IF PC-CNT-NUM(IDX) = RR-TEMP-LEN
               MOVE "Y" TO RR-TEMP-FLAG
               MOVE "," TO RR-PRE-FLAG
               *> 若此欄位+1/2/3格為國家 塞入郵遞區號
               IF ((IDX NOT EQUAL TO 30 AND PC-PART-CHECK(IDX + 1) = 2)
                   OR
                   (IDX NOT EQUAL TO 29 AND PC-PART-CHECK(IDX + 2) = 2)
                   OR
                   (IDX NOT EQUAL TO 28 AND PC-PART-CHECK(IDX + 3) = 2))
                  AND NOT (PC-CNT-COMMA(IDX) = "," AND 
                           PC-CNT-NUM(IDX + 1) > 1)
                 MOVE 1 TO RR-DTLS-FLAG
               ELSE
                 MOVE 9 TO RR-DTLS-FLAG
               END-IF

               *> 荷蘭郵遞區號: 前半:4個數字，後半:大寫英文*2
               IF IDX NOT EQUAL TO 30 AND
                  PC-CNT-NUM(IDX)      = 4 AND
                  PC-CNT-CHAR(IDX + 1) = 2 AND
                  DTLS-LF(2) = "NL"
                 MOVE "Y" TO RR-NEXT-FLAG
                 MOVE 1   TO RR-DTLS-FLAG
               END-IF

               *> 若國家並非 摩納哥"MC" 或 丹麥"DK" 且 數字字數 <= 2
               *> ZIP -> FLOOR
               IF (DTLS-LF(2) NOT = "MC"  AND
                   DTLS-LF(2) NOT = "DK") AND
                  PC-CNT-NUM(IDX)   <= 2  AND
                  RR-DTLS-FLAG       = 1
                  MOVE 11 TO RR-DTLS-FLAG
                  IF DTLS-LF(9) = SPACES
                    MOVE 9 TO RR-DTLS-FLAG
                  END-IF

                  *> 若前一欄 被分類至 ZIP 且 TEMP 也是數字 需要互相串聯
                  IF PC-PART-CHECK(IDX - 1) = 1
                    MOVE DTLS-LF(1) TO MD-STRING-A
                    MOVE RR-TEMP-COL TO MD-STRING-B
                    MOVE RR-TEMP-LEN TO MD-LEN-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-FINISH TO RR-TEMP-COL
                    MOVE MD-L TO RR-TEMP-LEN

                    MOVE 1 TO RR-DTLS-FLAG
                    MOVE SPACES TO DTLS-LF(1)
                  END-IF
               END-IF
             END-IF

             IF RR-DTLS-FLAG = 1 AND PC-CNT-NUM(IDX + 1) > 0
               MOVE "Y" TO RR-NEXT-FLAG
             END-IF
             *> =================== NUMBER 判斷結束 ===================
             *> =================== FLOOR 判斷 ===================
             INITIALIZE RR-FLOOR
             INSPECT RR-NEXT-COL TALLYING RR-CNT-F FOR ALL "F."
             INSPECT RR-NEXT-COL TALLYING RR-CNT-FL FOR ALL "FL."
             IF PC-CNT-NUM(IDX) > 0 AND
               RR-CNT-F > 0 OR RR-CNT-FL > 0

               MOVE RR-TEMP-COL TO MD-STRING-A
               MOVE RR-NEXT-COL TO MD-STRING-B
               MOVE "N"         TO MD-FLAG
               PERFORM FA-MERGE-RTN
               MOVE MD-FINISH TO RR-TEMP-COL
               MOVE SPACES TO RR-NEXT-COL

               MOVE "Y" TO RR-NEXT-FLAG RR-TEMP-FLAG
               MOVE 11  TO RR-DTLS-FLAG
             END-IF
       
             IF RR-TEMP-COL(RR-TEMP-LEN:1) = "F" AND 
                RR-TEMP-COL(RR-TEMP-LEN - 1:1) IS NUMERIC
      *        MOVE RR-TEMP-COL(1 : RR-TEMP-LEN - 1) TO RR-TEMP-COL *> 省略
               MOVE "Y" TO RR-TEMP-FLAG
               MOVE 11  TO RR-DTLS-FLAG
             END-IF

      *       IF RR-TEMP-COL(RR-TEMP-LEN:2) = "F," AND 
      *           RR-TEMP-COL(RR-TEMP-LEN - 2:1) IS NUMERIC
      *         MOVE RR-TEMP-COL(1 : RR-TEMP-LEN - 2) TO RR-TEMP-COL
      *         MOVE "Y" TO RR-TEMP-FLAG
      *         MOVE 11  TO RR-DTLS-FLAG
      *       END-IF
             IF RR-TEMP-COL(RR-TEMP-LEN - 1:2) = "F." AND
                (PC-CNT-NUM(IDX + 1) = 0 OR
                PC-CNT-COMMA(IDX) = ",")
                MOVE "Y" TO RR-TEMP-FLAG
                MOVE "N" TO RR-NEXT-FLAG
                MOVE 11  TO RR-DTLS-FLAG
             END-IF

             IF  RR-TEMP-COL(1:1) = "B" AND
               ((RR-TEMP-COL(2:RR-TEMP-LEN - 1) IS NUMERIC) OR
                (RR-TEMP-COL(RR-TEMP-LEN:1) = "," AND
                 RR-TEMP-COL(2:RR-TEMP-LEN - 2) IS NUMERIC))
               MOVE "Y" TO RR-TEMP-FLAG
               MOVE 11  TO RR-DTLS-FLAG
             END-IF

             *> =================== FLOOR 判斷結束 ===================
             *> =================== 特殊POST-BOX 判斷 ===================
             *> 【991 BP 992】-> [ZIP] = 991/ [POST-BOX] = BP 992
             IF RR-DTLS-FLAG NOT = 1 AND
                PC-CNT-NUM(IDX) > 0 AND
                (RR-NEXT-COL = "BP" OR RR-NEXT-COL = "B.P." OR
                 RR-NEXT-COL = "P/BAG") AND
                PC-CNT-NUM(IDX + 2) > 0
               MOVE "Y" TO RR-TEMP-FLAG
               MOVE 1   TO RR-DTLS-FLAG
               MOVE "N" TO RR-NEXT-FLAG
             END-IF

             *> 若字串為 P.O. XXX 999 則 3欄位一同納入 12[POST-BOX]
             IF (RR-TEMP-COL(1:4) = "P.O." OR RR-TEMP-COL(1:3) = "PO-") 
                AND PC-CNT-NUM(IDX + 1) = 0
                AND PC-CNT-NUM(IDX + 2) > 0
               MOVE RR-NEXT-COL TO MD-STRING-A
               MOVE RR-NEXT-LEN TO MD-LEN-A
               MOVE WK-PART(IDX + 2) TO MD-STRING-B
               PERFORM FA-MERGE-RTN
               MOVE MD-FINISH TO RR-NEXT-COL
               MOVE MD-L TO RR-NEXT-LEN

               MOVE "Y" TO RR-TEMP-FLAG RR-NEXT-FLAG
               MOVE 12  TO RR-DTLS-FLAG PC-PART-CHECK(IDX + 2)
               ADD 1 TO RR-IDX-PLUS
               MOVE SPACES TO WK-PART(IDX + 2)
             END-IF

             *> =================== 特殊ZIP 判斷 ===================
             IF RR-DTLS-FLAG NOT = 1 AND
                *> 若判斷欄位後1~3位為國家(位於終盤)
                (PC-PART-CHECK(IDX + 1) = 2 OR
                 PC-PART-CHECK(IDX + 2) = 2 OR
                 PC-PART-CHECK(IDX + 3) = 2)
               PERFORM FA-ZIP
             END-IF

              *> =================== 特殊ZIP 判斷結束 ===================
              *> 是否為 號
              IF RR-TEMP-LEN - PC-CNT-NUM(IDX) = 1 AND
                 RR-TEMP-LEN > 1 AND PC-CNT-NUM(IDX) > 1 AND
                 RR-TEMP-COL(RR-TEMP-LEN:1) IS ALPHABETIC-UPPER AND
                 RR-DTLS-FLAG < 11
                MOVE "," TO RR-PRE-FLAG
                MOVE "Y" TO RR-TEMP-FLAG
                MOVE 9 TO RR-DTLS-FLAG
              END-IF

              *> 特殊 STREET 判斷
              *> 西班牙/葡萄牙/英文 日期大道{STREET} DD DE MM
                *> 例: 
                *> [STREET] -> [DAY] -> DE      -> [MONTH]  : Calle 1 de Enero
                *> [DAY]    -> OF    -> [MONTH] -> [STREET] : 4th of July Avenue
                *> [MONTH]  -> [DAY] -> [STREET]            : July 4th Avenue
                *> [STREET] -> DU    -> [DAY]   -> [MONTH]: Boulevard du 30 Juin
                *> 確認 RR-CHECK-COL 是否為 月份之西班牙/葡萄牙/英文
              MOVE "N" TO WK-TEMP-FLAG
              MOVE SPACES TO RR-CHECK-COL
              IF PC-CNT-KEY-I(IDX) = 5 AND
                 PC-CNT-NUM(IDX + 1) > 0 AND PC-CNT-NUM(IDX + 1) < 3 AND
                 WK-PART(IDX + 2) = "DE"
                   MOVE WK-PART(IDX + 3)(1:PC-CNT-CHAR(IDX + 3) 
                                  + PC-CNT-NUM(IDX + 3)) TO RR-CHECK-COL
                   MOVE 3 TO RR-CHECK-LEN
              END-IF

              IF PC-CNT-NUM(IDX) > 0 AND PC-CNT-NUM(IDX) < 3 AND
                 WK-PART(IDX + 1) = "OF" AND
                 PC-CNT-KEY-I(IDX + 3) = 5
                   MOVE WK-PART(IDX + 2)(1:PC-CNT-CHAR(IDX + 2) 
                                  + PC-CNT-NUM(IDX + 2)) TO RR-CHECK-COL
                   MOVE 3 TO RR-CHECK-LEN
              END-IF

              IF PC-CNT-NUM(IDX + 1) > 0 AND PC-CNT-NUM(IDX + 1) < 3 AND
                 PC-CNT-KEY-I(IDX + 2) = 5
                   MOVE RR-TEMP-COL TO RR-CHECK-COL
                   MOVE 2 TO RR-CHECK-LEN
              END-IF

              IF PC-CNT-KEY-I(IDX) = 5 AND
                 RR-NEXT-COL = "DU" AND
                 PC-CNT-NUM(IDX + 2) > 0 AND PC-CNT-NUM(IDX + 2) < 3
                   MOVE WK-PART(IDX + 3)(1:PC-CNT-CHAR(IDX + 3) 
                                  + PC-CNT-NUM(IDX + 3)) TO RR-CHECK-COL
                   MOVE 3 TO RR-CHECK-LEN
              END-IF

              EVALUATE RR-CHECK-COL
                WHEN "JANUARY"
                WHEN "JANEIRO"
                WHEN "ENERO"

                WHEN "FEBRUARY"
                WHEN "FEVEREIRO"
                WHEN "FEBRERO"

                WHEN "MARCH"
                WHEN "MARCO"
                WHEN "MARZO"

                WHEN "APRIL"
                WHEN "ABRIL"

                WHEN "MAY"
                WHEN "MAIO"
                WHEN "MAYO"

                WHEN "JUNE"
                WHEN "JUIN"
                WHEN "JUNHO"
                WHEN "JUNIO"

                WHEN "JULY"
                WHEN "JULHO"
                WHEN "JULIO"

                WHEN "AUGUST"
                WHEN "AGOSTO"

                WHEN "SEPTEMBER"
                WHEN "SETEMBRO"
                WHEN "SEPTIEMBRE"

                WHEN "OCTOBER"
                WHEN "OUTUBRO"
                WHEN "OCTUBRE"

                WHEN "NOVEMBER"
                WHEN "NOVEMBRO"
                WHEN "NOVIEMBRE"

                WHEN "DECEMBER"
                WHEN "DEZEMBRO"
                WHEN "DICIEMBRE"
                  MOVE "Y" TO WK-TEMP-FLAG
              END-EVALUATE
              IF TEMP-FLAG-Y AND RR-CHECK-COL NOT = SPACES
                  PERFORM VARYING JDX FROM 1 BY 1
                                             UNTIL JDX > RR-CHECK-LEN
                   MOVE RR-TEMP-LEN TO MD-LEN-A
                   MOVE RR-TEMP-COL TO MD-STRING-A
                   COMPUTE MD-LEN-B = PC-CNT-CHAR(IDX + JDX) + 
                                      PC-CNT-NUM(IDX + JDX)
                   MOVE WK-PART(IDX + JDX)(1:MD-LEN-B) TO MD-STRING-B
                   PERFORM FA-MERGE-RTN
                   MOVE MD-FINISH TO RR-TEMP-COL
                   MOVE MD-L TO RR-TEMP-LEN
                   MOVE 5 TO PC-PART-CHECK(IDX + JDX)
                   MOVE SPACES TO WK-PART(IDX + JDX)
                   ADD 1 TO RR-IDX-PLUS
                  END-PERFORM
                  MOVE SPACES TO RR-NEXT-COL
                  MOVE "Y" TO RR-TEMP-FLAG
                  MOVE 5 TO RR-DTLS-FLAG
                END-IF
              *> =================== LS-LIST-COL 判斷 ===================
              IF RR-TEMP-FLAG NOT = "Y" OR
                 (DTLS-LF(RR-DTLS-FLAG) NOT = SPACES AND 
                 RR-DTLS-FLAG NOT = 11)

                MOVE PC-CNT-KEY-I(IDX) TO JDX
                MOVE PC-CNT-KEY-W(IDX) TO RR-CHECK-COL
                MOVE LENGTH OF FUNCTION TRIM(RR-CHECK-COL)
                                          TO RR-CHECK-LEN
                *> ===== 羅馬字 判斷 ====
                IF JDX > 30 AND JDX < 60
                    MOVE "," TO RR-PRE-FLAG
                    MOVE "Y" TO RR-TEMP-FLAG
                    SUBTRACT 30 FROM JDX GIVING JDX RR-DTLS-FLAG
                END-IF

                *> ===== 後接字 判斷 ====
                IF JDX > 60 AND JDX < 99

                    MOVE "," TO RR-PRE-FLAG
                    MOVE "Y" TO RR-TEMP-FLAG
                    SUBTRACT 60 FROM JDX GIVING JDX RR-DTLS-FLAG
                                                PC-PART-CHECK(IDX + 1)

                    COMPUTE MD-LEN-A = RR-CHECK-LEN - 1
                    MOVE RR-TEMP-COL(1:RR-CHECK-LEN - 1) TO MD-STRING-A
                    MOVE RR-NEXT-LEN TO MD-LEN-B
                    MOVE RR-NEXT-COL(1:RR-NEXT-LEN) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-FINISH TO RR-TEMP-COL

                    MOVE SPACES TO RR-NEXT-COL WK-PART(IDX + 1)
                    MOVE 1 TO RR-IDX-PLUS

                    *> 義大利街名判斷
                    PERFORM VARYING RR-CNT-F FROM 2 BY 1
                            UNTIL DTLS-LF(2) = "IT" AND JDX = 5 AND
                                 (PC-CNT-COMMA(IDX + RR-CNT-F - 1) = ","
                               OR PC-CNT-NUM(IDX + RR-CNT-F) > 0
                               OR PC-PART-CHECK(IDX) NOT = 0)

                      MOVE RR-TEMP-COL TO MD-STRING-A
                      MOVE WK-PART(IDX + RR-CNT-F) TO MD-STRING-B
                      PERFORM FA-MERGE-RTN
                      MOVE MD-FINISH TO RR-TEMP-COL

                      MOVE SPACES TO RR-NEXT-COL WK-PART(IDX + RR-CNT-F)
                      MOVE JDX TO RR-DTLS-FLAG
                                  PC-PART-CHECK(IDX + RR-CNT-F)
                      ADD 1 TO RR-IDX-PLUS
                    END-PERFORM

                END-IF

                *> ===== 段巷弄號樓室 判斷 =====
                IF JDX >= 6 AND JDX <= 16 AND
                   RR-TEMP-COL = RR-CHECK-COL
      *          OR RR-TEMP-COL(RR-TEMP-LEN - RR-CHECK-LEN + 1
      *                         :RR-CHECK-LEN) = RR-CHECK-COL)
                  MOVE "Y" TO RR-TEMP-FLAG
                  MOVE JDX TO RR-DTLS-FLAG

                  *> 若 TEMP 為關鍵字但無其他資訊(需追加 NEXT 資訊)
                  IF RR-PRE-COL = SPACES AND RR-TEMP-COL = RR-CHECK-COL
                    AND NOT((JDX >= 3 AND JDX <= 5) OR JDX = 10 OR
                             JDX >= 15)
                    AND RR-TEMP-COL NOT = "M/F"
                    MOVE "Y" TO RR-NEXT-FLAG

                    *> BASEMENT 1 -> B1F
                    IF RR-TEMP-COL = "BASEMENT"
                      MOVE "B" TO RR-TEMP-COL
                      MOVE RR-NEXT-COL(1:RR-NEXT-LEN)
                                      TO RR-TEMP-COL(2:RR-NEXT-LEN)
                      MOVE "F" TO RR-TEMP-COL(RR-NEXT-LEN + 2:1)

                      MOVE RR-TEMP-COL TO TD-TEMP
                      PERFORM FA-TRIM
                      MOVE TD-L TO RR-TEMP-LEN
                      MOVE SPACES TO RR-NEXT-COL
                      MOVE 0 TO RR-NEXT-LEN
                    END-IF
      *           ELSE
      *            *> 若 TEMP 為關鍵字但無其他資訊(該關鍵字不能在字串首位)
      *             MOVE "N" TO RR-TEMP-FLAG RR-NEXT-FLAG
      *             MOVE 0 TO RR-DTLS-FLAG
                  END-IF

                  IF RR-TEMP-COL = "M/F"
                    MOVE "Y" TO RR-TEMP-FLAG
                    MOVE JDX TO RR-DTLS-FLAG
                  END-IF

                  IF RR-TEMP-COL = RR-CHECK-COL AND RR-DTLS-FLAG = 0
                    MOVE "Y" TO RR-TEMP-FLAG
                    MOVE JDX TO RR-DTLS-FLAG
                  END-IF
                END-IF

                *> ===== TEMP 為關鍵字 但無前接 且後接為數字判斷 =====
                IF RR-TEMP-FLAG = "N" AND
                   RR-PRE-COL = SPACES AND
                   PC-CNT-NUM(IDX + 1) > 0 AND
                   PC-CNT-NUM(IDX + 1) <= 3 AND
                   JDX > 0 AND JDX < 30 AND
                   PC-CNT-COMMA(IDX) NOT = ","
                  MOVE JDX TO RR-DTLS-FLAG
                  MOVE "Y" TO RR-TEMP-FLAG RR-NEXT-FLAG
                END-IF

                *> ===== TEMP 為關鍵字 但無前接 且 JDX對應欄位已有值=====
                IF RR-TEMP-FLAG = "N" AND
                   RR-PRE-COL = SPACES AND
                   DTLS-LF(JDX) NOT = SPACES AND
                   JDX > 0 AND JDX < 30 AND
                   PC-CNT-COMMA(IDX - 1) NOT = ","
                     MOVE DTLS-LF(JDX) TO MD-STRING-A
                     MOVE RR-TEMP-LEN TO MD-LEN-B
                     MOVE RR-TEMP-COL TO MD-STRING-B
                     PERFORM FA-MERGE-RTN
                     MOVE MD-FINISH TO RR-TEMP-COL

                     MOVE SPACES TO DTLS-LF(JDX)
                     MOVE JDX TO RR-DTLS-FLAG
                     MOVE "Y" TO RR-TEMP-FLAG
                END-IF

           *> ===== (RR-NEXT-COL)特定字判斷 =====
                *> 若找到相符內容
      *      DISPLAY "RR-NEXT-FLAG? "RR-NEXT-FLAG"\"
      *      FUNCTION TRIM(RR-NEXT-COL)"\"
      *      FUNCTION TRIM(RR-CHECK-COL)"\"JDX

                IF PC-CNT-KEY-I(IDX + 1) NOT = 0 AND
                   PC-CNT-KEY-I(IDX + 1) < 30 AND
                   PC-CNT-COMMA(IDX) NOT = ","
                  MOVE "Y" TO RR-TEMP-FLAG
                  MOVE "Y" TO RR-NEXT-FLAG
                  MOVE PC-CNT-KEY-I(IDX + 1) TO JDX RR-DTLS-FLAG

                  *> ===== 是否需要跳過處理 判斷 =====
                  *> EX 原邏輯: (XXX PARK) -> 4[DISTRICT] (RD.) -> 5[STREET]
                  *>    修正後: (XXX PARK RD.) -> 5[STREET]
                  IF PC-CNT-KEY-I(IDX + 2) = 5 AND
                     PC-CNT-COMMA(IDX + 1) NOT = ","
                    MOVE 5 TO RR-DTLS-FLAG
                    MOVE RR-TEMP-LEN TO MD-LEN-A
                    MOVE RR-TEMP-COL(1:RR-TEMP-LEN) TO MD-STRING-A
                    MOVE RR-NEXT-LEN TO MD-LEN-B
                    MOVE RR-NEXT-COL(1:RR-NEXT-LEN) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN

                    MOVE MD-L TO MD-LEN-A
                    MOVE MD-FINISH TO MD-STRING-A
                    MOVE PC-CNT-CHAR(IDX + 2) TO MD-LEN-B
                    MOVE WK-PART(IDX + 2) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN

                    MOVE MD-FINISH TO RR-TEMP-COL
                    MOVE SPACES TO WK-PART(IDX + 2) RR-NEXT-COL
                    MOVE 1 TO RR-IDX-PLUS
                  END-IF

                  *> FLOOR 字串判斷
                  IF JDX = 11
      *             *> "1ST FLOOR" -> "1" 省略贅字
      *             IF PC-CNT-NUM(IDX) > 0 AND RR-NEXT-COL = "FLOOR"
      *               INSPECT RR-TEMP-COL REPLACING ALL "ST" BY SPACES
      *               INSPECT RR-TEMP-COL REPLACING ALL "ND" BY SPACES
      *               INSPECT RR-TEMP-COL REPLACING ALL "RD" BY SPACES
      *               INSPECT RR-TEMP-COL REPLACING ALL "TH" BY SPACES
      *               MOVE SPACES TO RR-NEXT-COL
      *               MOVE 0 TO RR-NEXT-LEN
      *             END-IF

                    MOVE RR-PRE-LEN TO MD-LEN-A
                    MOVE RR-PRE-COL(1:RR-PRE-LEN) TO MD-STRING-A
                    MOVE RR-TEMP-COL TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-L TO MD-LEN-A
                    MOVE MD-FINISH TO MD-STRING-A
                    MOVE RR-NEXT-LEN TO MD-LEN-B
                    MOVE RR-NEXT-COL(1:RR-NEXT-LEN) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN

                    MOVE MD-FINISH TO RR-TEMP-COL
                    MOVE MD-L TO RR-TEMP-LEN
                    MOVE SPACES TO RR-PRE-COL RR-NEXT-COL
                    MOVE 0 TO RR-PRE-LEN RR-NEXT-LEN
                  ELSE *> FLOOR 字串判斷 結束

                  *> STREET 後接字串判斷
                  COMPUTE WK-TEMP-LEN = PC-CNT-NUM(IDX + 2) +
                                          PC-CNT-CHAR(IDX + 2)
                  MOVE WK-PART(IDX + 2)(1:WK-TEMP-LEN) 
                       TO WK-TEMP-COL
                    *> 全方向
                  IF (PC-CNT-KEY-I(IDX + 2) = 99 OR
                      PC-CNT-KEY-W(IDX + 2) = "LOOP") AND
                     PC-CNT-COMMA(IDX + 1)  NOT = ","
                       MOVE RR-NEXT-LEN TO MD-LEN-A
                       MOVE RR-NEXT-COL(1:RR-NEXT-LEN) TO MD-STRING-A
                       MOVE RR-TEMP-LEN TO MD-LEN-B
                       MOVE WK-TEMP-COL(1:WK-TEMP-LEN) TO MD-STRING-B
                       PERFORM FA-MERGE-RTN
                       MOVE MD-FINISH TO RR-NEXT-COL
                       MOVE MD-L TO RR-NEXT-LEN

                       MOVE 1 TO RR-IDX-PLUS
                    END-IF
                  END-IF

                  *> SECTION -> SUB-DEPARTMENT(若段為文字)
                  IF RR-DTLS-FLAG = 6 AND PC-CNT-NUM(IDX) = 0
                    MOVE 18 TO RR-DTLS-FLAG
                  END-IF

                  *> 若
                  IF RR-DTLS-FLAG = 13 AND PC-CNT-NUM(IDX) = 0
                    MOVE 18 TO RR-DTLS-FLAG
                  END-IF

                END-IF
           *> ===== (RR-NEXT-COL)特定字判斷 完成 =====
               IF (RR-DTLS-FLAG = 0 AND
                  (PC-CNT-KEY-I(IDX+ 1) = 99 OR
                   PC-CNT-KEY-W(IDX+ 1) = "LOOP")
                  AND
                  PC-CNT-COMMA(IDX) NOT = ",") OR
                  RR-NEXT-COL = "DE"   OR
                  RR-NEXT-COL = "DEL"

                  MOVE "Y" TO RR-TEMP-FLAG RR-NEXT-FLAG
                  MOVE 5   TO RR-DTLS-FLAG

                  *> (**** DIRECTION ROAD) OR
                  *> (VIA DIRECTION ****) OR
                  *> (**** DE ****) OR
                  *> (**** DEL ****)
                  IF WK-PART(IDX + 2)(1:4) = "ROAD" OR
                     WK-PART(IDX + 2)(1:3) = "RD."  OR
                     RR-TEMP-COL = "VIA"  OR
                     RR-NEXT-COL = "DE"   OR
                     RR-NEXT-COL = "DEL"
                    MOVE RR-TEMP-LEN TO MD-LEN-A
                    MOVE RR-TEMP-COL(1:RR-TEMP-LEN) TO MD-STRING-A
                    MOVE RR-NEXT-LEN TO MD-LEN-B
                    MOVE RR-NEXT-COL(1:RR-NEXT-LEN) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-FINISH TO MD-STRING-A
                    MOVE MD-L TO MD-LEN-A
                    MOVE PC-CNT-CHAR(IDX + 2) TO MD-LEN-B
                    MOVE WK-PART(IDX + 2)(1:MD-LEN-B) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-FINISH TO RR-TEMP-COL
                    MOVE MD-L TO RR-TEMP-LEN

                    MOVE SPACES TO WK-PART(IDX + 2) RR-NEXT-COL
                    MOVE 1 TO RR-IDX-PLUS
                  END-IF
               ELSE

               *> 若此欄為 VIA~ 且 NEXT無逗號 IDX + 2 欄位也需要加入
               IF RR-DTLS-FLAG = 5 AND RR-IDX-PLUS = 1 AND
                  RR-TEMP-COL(1:4) = "VIA " AND
                  PC-CNT-COMMA(IDX + 1) NOT = ","

                  MOVE RR-TEMP-COL TO MD-STRING-A
                  MOVE WK-PART(IDX + 2) TO MD-STRING-B
                  COMPUTE MD-LEN-B = PC-CNT-NUM(IDX + 2)
                                   + PC-CNT-CHAR(IDX + 2)
                  PERFORM FA-MERGE-RTN
                  MOVE MD-FINISH TO RR-TEMP-COL
                  MOVE SPACES TO WK-PART(IDX + 2) RR-NEXT-COL
                  ADD 1 TO RR-IDX-PLUS
               END-IF
               END-IF

              END-IF
              *> =================== LS-LIST-COL 判斷結束 ===================
               *> 若有數字且9[NUMBER]為空欄 視為NUMBER
               IF PC-CNT-NUM(IDX) > 0 AND DTLS-LF(9) = SPACES AND
                  RR-DTLS-FLAG = 0
                  MOVE 9 TO RR-DTLS-FLAG
                  MOVE "Y" TO RR-TEMP-FLAG
                  MOVE "," TO RR-PRE-FLAG
               END-IF

               *> 若 已分類完成 但下一欄為為 `OF` 則下 下下欄位一同納入
               IF RR-DTLS-FLAG NOT = 0 AND
                  *> NEXT = "OF "
                  (RR-NEXT-FLAG = "N" AND RR-NEXT-COL(1:3) = "OF " AND
                  PC-CNT-COMMA(IDX) NOT = ",") OR
                  *> NEXT + 1 = "OF "
                  (RR-NEXT-FLAG = "Y" AND
                   WK-PART(IDX + RR-IDX-PLUS + 2)(1:3) = "OF" AND
                   PC-CNT-COMMA(IDX + RR-IDX-PLUS + 1) NOT = ",")

                    IF RR-NEXT-COL(1:3) NOT = "OF "
                      MOVE " OF" TO RR-NEXT-COL(RR-NEXT-LEN + 1:3)
                      MOVE SPACES TO WK-PART(IDX + RR-IDX-PLUS + 2)
                      MOVE RR-DTLS-FLAG
                            TO PC-PART-CHECK(IDX + RR-IDX-PLUS + 2)
                      ADD 3 TO RR-NEXT-LEN
                      ADD 1 TO RR-IDX-PLUS
                    END-IF

                    ADD 1 TO RR-IDX-PLUS
                    PERFORM VARYING JDX FROM RR-IDX-PLUS BY 1 
                            UNTIL WK-PART(IDX + JDX + 1) = SPACES OR
                                  PC-CNT-COMMA(IDX + JDX) = ","   OR
                                  PC-CNT-KEY-I(IDX + JDX + 1) NOT = 0 OR
                                  PC-CNT-NUM(IDX + JDX + 1) > 0
                      ADD 1 TO RR-IDX-PLUS
                      MOVE RR-NEXT-LEN TO MD-LEN-A
                      MOVE RR-NEXT-COL TO MD-STRING-A
                      MOVE WK-PART(IDX + RR-IDX-PLUS) TO MD-STRING-B
                      PERFORM FA-MERGE-RTN
                      MOVE MD-FINISH TO RR-NEXT-COL
                      MOVE MD-L TO RR-NEXT-LEN

                      MOVE RR-DTLS-FLAG
                                  TO PC-PART-CHECK(IDX + RR-IDX-PLUS)
                      MOVE SPACES TO WK-PART(IDX + RR-IDX-PLUS)
                    END-PERFORM

                    IF RR-NEXT-COL(1:3) NOT = "OF "
                      SUBTRACT 1 FROM RR-IDX-PLUS
                    END-IF

                    MOVE "Y" TO RR-NEXT-FLAG
               END-IF

              *> 尚未分類完成 但本身便帶有","
              IF PC-CNT-COMMA(IDX) = "," AND RR-DTLS-FLAG = 0
      *         DISPLAY "OTHER!!!!" 
      *           FUNCTION TRIM(RR-PRE-COL)"/ "
      *           FUNCTION TRIM(RR-TEMP-COL)"/ "
      *           FUNCTION TRIM(RR-NEXT-COL)"/ "
      *           PC-MATCH-NEW"/ "IDX"/ "PC-PART-CHECK(PC-MATCH-NEW)

                MOVE "Y" TO RR-TEMP-FLAG

                *> 樓層 + 大樓
                IF IDX NOT = 1 AND PC-MATCH-NEW NOT = 0
                   AND PC-PART-CHECK(PC-MATCH-NEW) = 11
                  MOVE 14 TO RR-DTLS-FLAG
                END-IF

                *> 若尚未填入 路以上之欄位
                IF RR-DTLS-FLAG = 0 AND DTLS-LF(5) = SPACES AND 
                  (PC-MATCH-NEW = 0 OR
                   PC-PART-CHECK(PC-MATCH-NEW) > 5 OR
                   PC-PART-CHECK(PC-MATCH-NEW) <= 2)

                  *> STREET 後接字串判斷
                  IF PC-CNT-COMMA(IDX + 1) = ","
                      MOVE RR-NEXT-COL(1:RR-NEXT-LEN - 1) 
                        TO RR-NEXT-COL
                  END-IF
                  MOVE 5 TO RR-DTLS-FLAG

                  *> 全方向
                  IF PC-CNT-COMMA(IDX) NOT = "," AND
                     PC-CNT-KEY-I(IDX + 1) = 99 OR
                     PC-CNT-KEY-W(IDX+ 1) = "LOOP"

                     IF PC-CNT-COMMA(IDX + 1) NOT = "," AND
                        PC-CNT-KEY-I(IDX + 2) > 0 AND
                        PC-CNT-KEY-I(IDX + 2) < 30
                       MOVE PC-CNT-KEY-I(IDX + 2) TO RR-DTLS-FLAG
                       ADD 1 TO RR-IDX-PLUS

                       MOVE RR-NEXT-COL TO MD-STRING-A
                       MOVE RR-NEXT-LEN TO MD-LEN-A
                       MOVE WK-PART(IDX + 2) TO MD-STRING-B
                       PERFORM FA-MERGE-RTN
                       MOVE MD-FINISH TO RR-NEXT-COL
                       MOVE MD-L TO RR-NEXT-LEN
                       
                       MOVE SPACES TO WK-PART(IDX + 2)
                     END-IF
                    MOVE "Y" TO RR-NEXT-FLAG
                  END-IF

                END-IF

                  PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 1
                    IF PC-PART-CHECK(JDX - 1) = 0
                      MOVE SPACES TO WK-PART(JDX)
                      MOVE 98 TO PC-PART-CHECK(JDX)
                    END-IF
                  END-PERFORM
              END-IF

       *>  ====================== 判斷結束 ======================
           *> RR-NEXT-FLAG
              IF RR-NEXT-Y
                   MOVE RR-TEMP-COL TO MD-STRING-A
                   MOVE RR-NEXT-COL TO MD-STRING-B
                   PERFORM FA-MERGE-RTN
                   MOVE MD-FINISH TO RR-TEMP-COL
                   MOVE MD-L TO RR-TEMP-LEN

                   MOVE SPACES TO WK-PART(IDX) WK-PART(IDX + 1)
                   MOVE RR-DTLS-FLAG TO PC-PART-CHECK(IDX)
                                        PC-PART-CHECK(IDX + 1)
                   ADD 1 TO IDX
              END-IF

              IF IDX = 1 OR RR-PRE-LEN = 0 OR
                 PC-CNT-COMMA(IDX - 1) = ","
                MOVE SPACES TO RR-PRE-FLAG
              END-IF

           *> CHECK RR-PRE-FLAG
           IF RR-PRE-LEN > 0
             IF (RR-PRE-COL(RR-PRE-LEN:1) = "," OR RR-PRE-Y)
               AND RR-DTLS-FLAG NOT = 0 
             IF DTLS-LF(14) = SPACES AND
                (PC-MATCH-NEW >= 1 AND PC-MATCH-NEW <= 30) AND
                PC-PART-CHECK(PC-MATCH-NEW) = 11
               MOVE RR-PRE-COL TO DTLS-LF(14)
             ELSE
             *> 若 STREET 欄位為空值 且 (非倒數第1~3位 或 目前找到 9[NUMBER])
             IF DTLS-LF(5) = SPACES AND
                (PC-PART-CHECK(IDX + 1) NOT = 2 AND
                 PC-PART-CHECK(IDX + 2) NOT = 2 AND
                 PC-PART-CHECK(IDX + 3) NOT = 2 ) OR
                 RR-DTLS-FLAG = 9
               MOVE RR-PRE-COL TO DTLS-LF(5)
             ELSE
               MOVE RR-PRE-COL TO WK-PART(IDX - 1)

      *        *> 清理WK-PART
      *        IF IDX > 2
      *          PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 2
      *            IF PC-PART-CHECK(JDX - 2) = 0
      *              MOVE SPACES TO WK-PART(JDX - 2)
      *              MOVE 97 TO PC-PART-CHECK(JDX - 2)
      *            END-IF
      *          END-PERFORM
      *        END-IF
             END-IF
               MOVE SPACES TO RR-PRE-COL RR-PRE-FLAG
               MOVE 0 TO RR-PRE-LEN
             END-IF
           END-IF
           END-IF

           *> 串聯
              MOVE RR-PRE-COL TO MD-STRING-A
              MOVE RR-TEMP-COL TO MD-STRING-B
              IF RR-PRE-Y
                MOVE "," TO MD-COL
              END-IF
              PERFORM FA-MERGE-RTN
              MOVE MD-FINISH TO RR-PRE-COL
              MOVE MD-L TO RR-PRE-LEN

           *> RR-DTLS-FLAG判斷
           IF RR-DTLS-FLAG NOT = 0
              MOVE RR-PRE-COL TO TD-TEMP
              PERFORM FA-TRIM
              MOVE TD-FINISH TO RR-PRE-COL
              MOVE TD-L TO RR-PRE-LEN
             IF RR-PRE-COL(RR-PRE-LEN:1) = ","
               MOVE RR-PRE-COL(1:RR-PRE-LEN - 1) TO RR-PRE-COL
             END-IF

             *> 若欲移動之位子已經有值 原本要移動至 DEPARTMENT(10)
             *> 英文撰寫順序: 公司名 -> 部門
             IF DTLS-LF(RR-DTLS-FLAG) NOT = SPACES AND
                RR-DTLS-FLAG = 10 AND RR-TEMP-FLAG = "Y"
                    MOVE 18 TO RR-DTLS-FLAG
             END-IF

             *> 若欲移動之位子已經有值 原本要移動至 BUILDING(14)
             *> 英文撰寫順序: 建築大樓(單棟) -> 建築大樓(區域)
             IF DTLS-LF(RR-DTLS-FLAG) NOT = SPACES AND
                RR-DTLS-FLAG = 14 AND
                RR-TEMP-FLAG = "Y"

                MOVE DTLS-LF(14) TO TD-TEMP
                PERFORM FA-TRIM
                IF DTLS-LF(14)(TD-L:1) NOT = ","
                  MOVE "," TO MD-COL
                END-IF

                MOVE DTLS-LF(RR-DTLS-FLAG) TO MD-STRING-A
                MOVE RR-PRE-LEN TO MD-LEN-B
                MOVE RR-PRE-COL TO MD-STRING-B
                PERFORM FA-MERGE-RTN
                MOVE MD-FINISH TO DTLS-LF(RR-DTLS-FLAG)

                MOVE DTLS-LF(RR-DTLS-FLAG) TO RR-PRE-COL
                MOVE SPACES TO DTLS-LF(RR-DTLS-FLAG)
             END-IF

             *> 若欲移動之位子已經有值 且 5[STREET]無值
             IF DTLS-LF(RR-DTLS-FLAG) NOT = SPACES AND
                DTLS-LF(5) = SPACES AND RR-TEMP-FLAG = "Y"
                  *> 若原本要移動至 NUMBER(9)
                  IF RR-DTLS-FLAG = 9
                    *> 若該值為數字 移至樓層 否則 移至建築物名稱
                    IF (PC-MATCH-NEW >= 1 AND PC-MATCH-NEW <= 30) AND
                       PC-CNT-NUM(PC-MATCH-NEW) > 2 AND
                       PC-CNT-CHAR(PC-MATCH-NEW) < 2
                       AND DTLS-LF(11) = SPACES
                      MOVE DTLS-LF(9) TO DTLS-LF(11)
                      MOVE SPACES TO DTLS-LF(9)
                    ELSE
                      MOVE DTLS-LF(9) TO DTLS-LF(14)
                      MOVE SPACES TO DTLS-LF(9)
                    END-IF
                  *> STREET
                  ELSE
                    MOVE 5 TO RR-DTLS-FLAG
                  END-IF
             ELSE
                  *> 若原本要移動至 NUMBER(9) 且 5[STREET]已有值
                  IF RR-DTLS-FLAG = 9 AND DTLS-LF(9) NOT = SPACES
                     AND DTLS-LF(5) NOT = SPACES 
                     AND PC-PART-CHECK(PC-MATCH-NEW) = 9

                     MOVE DTLS-LF(5) TO MD-STRING-A
                     MOVE DTLS-LF(9) TO MD-STRING-B
                     PERFORM FA-MERGE-RTN
                     MOVE MD-FINISH TO DTLS-LF(5)
                     MOVE SPACES TO DTLS-LF(9)
                  END-IF

             *> 若本欄位移動至
             *> (18[SUB-DEPARTMENT] 但 10[DEPARTMENT] 為空欄) 或
             *> 本欄位移動至 10[DEPARTMENT]
             *> 但前一位未分類完成/被分類至5[STREET] 塞回 10[DEPARTMENT]
             IF (RR-DTLS-FLAG = 18 AND DTLS-LF(10) = SPACES) OR
                 RR-DTLS-FLAG = 10

                IF IDX > 1 AND RR-NEXT-N AND PC-PART-CHECK(IDX - 1) = 5
                  MOVE WK-PART(IDX - 1) TO DTLS-LF(10)
                  IF DTLS-LF(10) = SPACES
                    MOVE DTLS-LF(5) TO DTLS-LF(10)
                  END-IF
                  MOVE SPACES TO WK-PART(IDX - 1) DTLS-LF(5)
                  MOVE 10 TO PC-PART-CHECK(IDX - 1)
                  MOVE 18 TO RR-DTLS-FLAG
                END-IF
                *> 若 IDX - 2 欄位 為5[STREET] 且 該欄位是依位置推論而塞入
                IF IDX > 2 AND RR-NEXT-Y AND PC-PART-CHECK(IDX - 2) = 5
                   AND PC-CNT-KEY-I(IDX - 2) NOT = 5
                  MOVE WK-PART(IDX - 2) TO DTLS-LF(10)
                  IF DTLS-LF(10) = SPACES
                    MOVE DTLS-LF(5) TO DTLS-LF(10)
                  END-IF
                  MOVE SPACES TO WK-PART(IDX - 2) DTLS-LF(5)
                  MOVE 10 TO PC-PART-CHECK(IDX - 2)
                  MOVE 18 TO RR-DTLS-FLAG
                END-IF
             END-IF

             *> 若5[STREET]已有值 且
             *> CNT判斷時 已經確認要放入5 DTLS-LF(5)-> DTLS-LF(10/ 18)
             INSPECT RR-PRE-COL(1:RR-PRE-LEN)
                     TALLYING RR-CNT-FL FOR ALL " "
             COMPUTE KDX = IDX - RR-CNT-FL
             MOVE 0 TO RR-TEMP-A-LEN RR-TEMP-B-LEN
             PERFORM VARYING JDX FROM KDX BY -1 UNTIL JDX = 1
               IF PC-CNT-KEY-I(JDX) > 30 AND PC-CNT-KEY-I(JDX) < 99
                 SUBTRACT 30 FROM PC-CNT-KEY-I(JDX)
               END-IF
               IF PC-CNT-KEY-I(JDX) > 30 AND PC-CNT-KEY-I(JDX) < 99
                 SUBTRACT 30 FROM PC-CNT-KEY-I(JDX)
               END-IF

               IF PC-CNT-KEY-I(JDX) > 1 AND PC-CNT-KEY-I(JDX) < 99 AND
                  PC-CNT-KEY-I(JDX) = PC-PART-CHECK(JDX)
                  IF RR-TEMP-A-LEN = 0
                    MOVE PC-CNT-KEY-I(JDX) TO RR-TEMP-A-LEN
                  ELSE
                    MOVE PC-CNT-KEY-I(JDX) TO RR-TEMP-B-LEN
                    EXIT PERFORM
                  END-IF
               END-IF
             END-PERFORM

             IF RR-DTLS-FLAG = 5 AND DTLS-LF(RR-DTLS-FLAG) NOT = SPACES
                IF PC-CNT-KEY-I(IDX) = 5
                    IF RR-TEMP-A-LEN = 5
                      MOVE DTLS-LF(5) TO MD-STRING-A
                      MOVE RR-PRE-LEN TO MD-LEN-B
                      MOVE RR-PRE-COL TO MD-STRING-B
                      MOVE "," TO MD-COL
                      PERFORM FA-MERGE-RTN
                      MOVE MD-FINISH TO DTLS-LF(5) RR-PRE-COL
                    ELSE
                    IF DTLS-LF(10) NOT = SPACES
                      MOVE DTLS-LF(5) TO DTLS-LF(18)
                    ELSE
                      MOVE DTLS-LF(5) TO DTLS-LF(10)
                    END-IF
                    MOVE SPACES TO DTLS-LF(5)
                    END-IF
                ELSE

                *> 若 CNT KEYWORD 判斷時雙方皆為5[STREET]
                IF (RR-TEMP-A-LEN = 5 OR RR-TEMP-B-LEN = 5) AND (
                   PC-CNT-KEY-I(KDX + 1) = 5 OR PC-CNT-KEY-I(IDX) = 5)
                     MOVE DTLS-LF(5) TO MD-STRING-A
                     MOVE "," TO MD-COL
                     MOVE RR-PRE-LEN TO MD-LEN-B
                     MOVE RR-PRE-COL TO MD-STRING-B
                     PERFORM FA-MERGE-RTN
                     MOVE MD-FINISH TO RR-PRE-COL
                     MOVE SPACES TO DTLS-LF(5)
                ELSE
                  *> 4[DISTRICT]無值
                  IF DTLS-LF(4) = SPACES
                      MOVE 4 TO RR-DTLS-FLAG
                  END-IF
                END-IF
                END-IF
             END-IF
             END-IF

             IF DTLS-LF(RR-DTLS-FLAG) = SPACES
               MOVE RR-PRE-COL TO TD-TEMP
               PERFORM FA-TRIM
               MOVE TD-FINISH TO DTLS-LF(RR-DTLS-FLAG)

               MOVE RR-DTLS-FLAG TO PC-PART-CHECK(IDX)
               MOVE IDX TO PC-MATCH-NEW
               MOVE SPACES TO WK-PART(IDX)
             END-IF

             *> 清理WK-PART
             IF IDX > 2
      *       AND RR-PRE-COL(1 : LENGTH OF FUNCTION TRIM(
      *          WK-PART(IDX))) NOT = WK-PART(IDX)
              MOVE 1 TO KDX
              MOVE RR-PRE-COL TO TD-TEMP
              PERFORM FA-TRIM
              MOVE TD-L TO RR-PRE-LEN

              PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > RR-PRE-LEN
                IF RR-PRE-COL(JDX:1) = " "
                  ADD 1 TO KDX
                END-IF
              END-PERFORM

               PERFORM VARYING JDX FROM 0 BY 1 UNTIL JDX = KDX
                 COMPUTE RR-CNT-F = IDX + RR-IDX-PLUS - JDX
                 IF PC-PART-CHECK(RR-CNT-F) = 0
                   MOVE SPACES TO WK-PART(RR-CNT-F)
                   MOVE 96 TO PC-PART-CHECK(RR-CNT-F)
                 ELSE
                   MOVE PC-PART-CHECK(IDX) TO
                        PC-PART-CHECK(RR-CNT-F)
                 END-IF
               END-PERFORM
             END-IF
           END-IF

           *> 納入 WK-PART(IDX)
           IF RR-TEMP-FLAG = "Y" AND RR-DTLS-FLAG  = 0
              MOVE RR-PRE-COL TO TD-TEMP
              PERFORM FA-TRIM
              MOVE TD-FINISH TO RR-PRE-COL WK-PART(IDX)
           ELSE

           *> 若前一欄為 99，將 RR-PRE-COL 放回 WK-PART
           IF IDX > 1 AND PC-PART-CHECK(IDX - 1) = 99
              AND NOT RR-TEMP-Y
             MOVE RR-PRE-COL TO WK-PART(IDX)
           END-IF
           END-IF

      *     *> ","插入判斷結果
      *    DISPLAY "IDX : "IDX "/ " RR-TEMP-FLAG"/ "RR-IDX-PLUS
      *    DISPLAY "CNT : "PC-CNT-NUM(IDX)"/ "PC-CNT-NUM(IDX + 1)
      *    DISPLAY "PRE : "FUNCTION TRIM(RR-PRE-COL)"/ "RR-PRE-LEN"/ "
      *                 RR-PRE-FLAG"/"FUNCTION TRIM(WK-PART(IDX - 1))
      *    DISPLAY "TEMP: "
      *            FUNCTION TRIM(RR-TEMP-COL) "/ " RR-TEMP-LEN
      *            "/ "PC-CNT-NUM(IDX)
      *            "/"FUNCTION TRIM(WK-PART(IDX))"/"
      *            PC-PART-CHECK(IDX)
      *    DISPLAY "MIX : "RR-NEXT-FLAG
      *    DISPLAY "NEXT: "
      *          FUNCTION TRIM(RR-NEXT-COL)"/ "RR-NEXT-LEN
      *          "/ "PC-CNT-NUM(IDX + 1)
      *    DISPLAY "DTLS:"RR-DTLS-FLAG"/ "
      *             FUNCTION TRIM(DTLS-LF(RR-DTLS-FLAG))
      *    DISPLAY PC-MATCH-NEW"/ "PC-PART-CHECK(IDX)
      *    DISPLAY "------------- ------------- -------------"

           ADD RR-IDX-PLUS TO IDX
           EVALUATE PC-PART-CHECK(IDX)
             WHEN 3
               MOVE IDX TO PC-OTHER-CITY
             WHEN 4
               MOVE IDX TO PC-OTHER-DISTRICT
             WHEN 5
               MOVE IDX TO PC-OTHER-STREET
             WHEN 16
               MOVE IDX TO PC-OTHER-PROVINCE
             WHEN 17
               MOVE IDX TO PC-OTHER-STATE
      *         WHEN OTHER
           END-EVALUATE
           END-PERFORM.
      *> ===================== OTHEER =====================
           *> P.O.-BOX -> P.O. Box
           INSPECT DTLS-LF(12) REPLACING ALL "P.O.-BOX"
                                          BY "P.O. Box".
           *> PO-BOX -> PO Box
           INSPECT DTLS-LF(12) REPLACING ALL "PO-BOX"
                                          BY "PO Box".
           *> PRIVATE-BAG -> PRIVATE BAG
           INSPECT DTLS-LF(12) REPLACING ALL "PRIVATE-BAG"
                                          BY "PRIVATE BAG".

      *> ============= 依照相對位置選擇插入欄位 =============
      *     *> 99: NOT RR-TEMP-Y  -> PC-PART-CHECK(KDX) = 99
      *    PERFORM VARYING KDX FROM 1 BY 1 UNTIL KDX > 23
      *      DISPLAY KDX" KDX: "FUNCTION TRIM(DTLS-LF(KDX))
      *      "/ TEMP: "FUNCTION TRIM(WK-PART(KDX)) "/ "
      *                PC-PART-CHECK(KDX)
      *    END-PERFORM.

           *> PC-OTHER-PRE: 前單字塞入欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 30
             IF PC-PART-CHECK(IDX) < 30 AND 
                PC-PART-CHECK(IDX) > 0
                MOVE PC-PART-CHECK(IDX) TO PC-OTHER-PRE

           PERFORM VARYING JDX FROM IDX BY 1 UNTIL JDX > 29
             IF PC-PART-CHECK(JDX + 1) < 30 AND 
                PC-PART-CHECK(JDX + 1) > 0
                MOVE PC-PART-CHECK(JDX + 1) TO PC-OTHER-NEXT
                EXIT PERFORM 
             END-IF
           END-PERFORM

             END-IF

             IF WK-PART(IDX) NOT = SPACES
               MOVE "N" TO PC-OTHER-FLAG
               MOVE WK-PART(IDX) TO TD-TEMP
               PERFORM FA-TRIM
               MOVE TD-FINISH TO RR-TEMP-COL
               MOVE TD-L TO RR-TEMP-LEN
               IF RR-TEMP-COL(RR-TEMP-LEN:1) = ","
                 SUBTRACT 1 FROM RR-TEMP-LEN
                 MOVE RR-TEMP-COL(1:RR-TEMP-LEN) TO RR-TEMP-COL
               END-IF

             *> ============ of 開頭 ============
               IF RR-TEMP-COL(1:2) = "OF"
               PERFORM VARYING JDX FROM IDX BY -1 UNTIL JDX = 1
                 IF PC-PART-CHECK(JDX - 1) NOT = 0 AND
                    PC-PART-CHECK(JDX - 1) NOT = 99

                    MOVE DTLS-LF(PC-PART-CHECK(JDX - 1)) TO MD-STRING-A
                    MOVE RR-TEMP-B(1:RR-TEMP-B-LEN) TO MD-STRING-B
                    PERFORM FA-MERGE-RTN
                    MOVE MD-FINISH TO DTLS-LF(PC-PART-CHECK(JDX - 1))
                    MOVE "Y" TO PC-OTHER-FLAG
                    EXIT PERFORM
                 END-IF
               END-PERFORM
               END-IF

             *> ============ STATE(若判斷字串僅3個文字) ============
               IF RR-TEMP-COL IS ALPHABETIC AND RR-TEMP-LEN <= 3 AND
                  PC-OTHER-FLAG = "N" AND (PC-OTHER-PRE = 1 OR
                  PC-PART-CHECK(IDX + 1) = 2) AND
                  PC-OTHER-STATE NOT = 99
                 MOVE RR-TEMP-COL TO DTLS-LF(17)
                 MOVE "Y" TO PC-OTHER-FLAG
                 MOVE IDX TO PC-OTHER-STATE
               END-IF

             *> ============ CITY ============
               IF PC-OTHER-FLAG = "N" AND PC-OTHER-CITY = 0
                  AND IDX > PC-OTHER-STREET
                 MOVE RR-TEMP-COL TO DTLS-LF(3)
                 MOVE "Y" TO PC-OTHER-FLAG
                 MOVE IDX TO PC-OTHER-CITY
               END-IF

             *> ============ STATE ============
               *> 若前一欄位為 郵遞區號
               IF PC-OTHER-FLAG = "N" AND (PC-OTHER-PRE = 1 OR
                  PC-PART-CHECK(IDX + 1) = 1) AND
                  PC-OTHER-STATE NOT = 99
                 *> PROVINCE 若已經有值
                 IF PC-OTHER-STATE NOT = 0
                   IF IDX > PC-OTHER-STATE
                     IF PC-OTHER-PROVINCE NOT = 0
                       IF PC-OTHER-DISTRICT  NOT = 0
                         MOVE DTLS-LF(4)    TO DTLS-LF(3)
                         MOVE PC-OTHER-DISTRICT TO PC-OTHER-CITY
                       END-IF
                       MOVE DTLS-LF(16)    TO DTLS-LF(4)
                       MOVE PC-OTHER-PROVINCE TO PC-OTHER-DISTRICT
                     END-IF
                     MOVE DTLS-LF(17)    TO DTLS-LF(16)
                     MOVE PC-OTHER-STATE TO PC-OTHER-PROVINCE
                   END-IF
                 END-IF

                 MOVE RR-TEMP-COL TO DTLS-LF(17)
                 MOVE "Y" TO PC-OTHER-FLAG
                 MOVE IDX TO PC-OTHER-STATE
               END-IF

             *> ============ PROVINCE ============
               IF PC-OTHER-FLAG = "N" AND IDX > PC-OTHER-PROVINCE
                 IF PC-OTHER-PROVINCE = 0
                   MOVE RR-TEMP-COL TO DTLS-LF(16)
                   MOVE IDX TO PC-OTHER-PROVINCE
                   MOVE "Y" TO PC-OTHER-FLAG
                 END-IF
                 IF PC-OTHER-FLAG = "N" AND PC-OTHER-CITY = 0
                   MOVE DTLS-LF(16) TO DTLS-LF(3)
                   MOVE PC-OTHER-PROVINCE TO PC-OTHER-CITY
                   MOVE RR-TEMP-COL TO DTLS-LF(16)
                   MOVE IDX TO PC-OTHER-PROVINCE
                   MOVE "Y" TO PC-OTHER-FLAG
                 ELSE
                   IF PC-OTHER-FLAG = "N" AND PC-OTHER-DISTRICT = 0
                     MOVE DTLS-LF(3) TO DTLS-LF(4)
                     MOVE PC-OTHER-CITY TO PC-OTHER-DISTRICT

                     MOVE DTLS-LF(16) TO DTLS-LF(3)
                     MOVE PC-OTHER-PROVINCE TO PC-OTHER-CITY
                     MOVE RR-TEMP-COL TO DTLS-LF(16)
                     MOVE IDX TO PC-OTHER-PROVINCE
                     MOVE "Y" TO PC-OTHER-FLAG
                   END-IF
                 END-IF
               END-IF

             *> ============ STATE ============
               IF PC-OTHER-FLAG = "N" AND IDX > PC-OTHER-PROVINCE AND
                  PC-OTHER-STATE = 0
                     MOVE RR-TEMP-COL TO DTLS-LF(17)
                     MOVE IDX TO PC-OTHER-STATE
                     MOVE "Y" TO PC-OTHER-FLAG
               END-IF

             *> ============ STREET ============
               IF PC-OTHER-FLAG = "N" AND PC-OTHER-STREET = 0
                 MOVE RR-TEMP-COL TO DTLS-LF(5)
                 *> 若 目前欄位 在 目前CITY欄位內容 的右邊
                 IF PC-OTHER-CITY > 0 AND IDX > PC-OTHER-CITY
                   MOVE DTLS-LF(3) TO DTLS-LF(5)
                   MOVE RR-TEMP-COL TO DTLS-LF(3)
                   MOVE PC-OTHER-CITY TO PC-OTHER-STREET
                   MOVE IDX TO PC-OTHER-CITY
                 END-IF
                 MOVE "Y" TO PC-OTHER-FLAG
                 MOVE 5 TO PC-OTHER-PRE
               END-IF

             *> ============ VILLAGE -> OTHER ============
               IF PC-OTHER-FLAG = "N" AND DTLS-LF(15) = SPACES
                   MOVE RR-TEMP-COL TO DTLS-LF(15)
                   MOVE "Y" TO PC-OTHER-FLAG
               END-IF
               IF PC-OTHER-FLAG = "N"
                   MOVE RR-TEMP-COL TO DTLS-LF(24)
               END-IF

               MOVE RR-DTLS-FLAG TO PC-OTHER-PRE

             END-IF

           END-PERFORM.

      *******************************************************
      *> FA-REBUILD SECTION 反結構
      *******************************************************
       FA-REBUILD.
           MOVE SPACES TO DTLS-LF(23).
      *>   設定順序
      *>   10[DEPARTMENT] -> 18[SUB-DEPARTMENT] ->
      *>   13[ROOM] → 11[FLOOR] → 14[BUILDING] → 9[NUMBER] →
      *>   8[ALLEY] → 7[LANE] → 6[SEC] → 5[SREET] →
      *>   15[VILLAGE] → 24[OTHER] → 12[POST-BOX] → 4[DISTRICT] →
      *>   3[CITY] → 16[PROVINCE] → 17[STATE] →
      *>   1[ZIP] → 2[COUNTRY]
           MOVE 10 TO RB-DTLS-LF(1).
           MOVE 18 TO RB-DTLS-LF(2).
       
           MOVE 13 TO RB-DTLS-LF(3).
           MOVE 11 TO RB-DTLS-LF(4).
           MOVE 14 TO RB-DTLS-LF(5).
           MOVE  9 TO RB-DTLS-LF(6).
       
           MOVE  8 TO RB-DTLS-LF(7).
           MOVE  7 TO RB-DTLS-LF(8).
           MOVE  6 TO RB-DTLS-LF(9).
           MOVE  5 TO RB-DTLS-LF(10).
       
           MOVE 15 TO RB-DTLS-LF(11).
           MOVE 24 TO RB-DTLS-LF(12).
           MOVE 12 TO RB-DTLS-LF(13).
           MOVE  4 TO RB-DTLS-LF(14).
       
           MOVE  3 TO RB-DTLS-LF(15).
           MOVE 16 TO RB-DTLS-LF(16).
           MOVE 17 TO RB-DTLS-LF(17).
       
           MOVE  1 TO RB-DTLS-LF(18).
           MOVE  2 TO RB-DTLS-LF(19).
       
           *> 串聯
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
             IF DTLS-LF(RB-DTLS-LF(IDX)) NOT = SPACES
               MOVE DTLS-LF(23) TO TD-TEMP
               PERFORM FA-TRIM
               MOVE TD-FINISH TO RR-TEMP-A
               MOVE TD-L TO RR-TEMP-A-LEN

               MOVE DTLS-LF(RB-DTLS-LF(IDX)) TO TD-TEMP
               PERFORM FA-TRIM
               MOVE TD-FINISH TO RR-TEMP-B
               MOVE TD-L TO RR-TEMP-B-LEN
               MOVE SPACES TO RR-TEMP-COL

               MOVE 0 TO RR-CNT-F RR-CNT-FL
               INSPECT RR-TEMP-B TALLYING RR-CNT-F FOR ALL "F"
               INSPECT RR-TEMP-B TALLYING RR-CNT-FL FOR ALL "LEVEL"

               IF RB-DTLS-LF(IDX) = 11 AND
                  RR-CNT-F = 0 AND RR-CNT-FL = 0
                 MOVE RR-TEMP-B(1:RR-TEMP-B-LEN) TO RR-TEMP-COL
                 MOVE "FLOOR" TO RR-TEMP-COL(RR-TEMP-B-LEN + 2:5)
                 MOVE RR-TEMP-COL TO RR-TEMP-B
                 ADD 6 TO RR-TEMP-B-LEN
               END-IF

               IF RB-DTLS-LF(IDX) = 8
                  AND RR-TEMP-B(1:RR-TEMP-B-LEN) IS NUMERIC
                 MOVE "ALY." TO RR-TEMP-COL(1:4)
                 MOVE RR-TEMP-B(1:RR-TEMP-B-LEN)
                                         TO RR-TEMP-COL(6:RR-TEMP-B-LEN)
                 MOVE RR-TEMP-COL TO RR-TEMP-B
                 ADD 5 TO RR-TEMP-B-LEN
               END-IF

               IF RB-DTLS-LF(IDX) = 7
                  AND RR-TEMP-B(1:RR-TEMP-B-LEN) IS NUMERIC
                 MOVE "LN." TO RR-TEMP-COL(1:3)
                 MOVE RR-TEMP-B(1:RR-TEMP-B-LEN)
                                         TO RR-TEMP-COL(5:RR-TEMP-B-LEN)
                 MOVE RR-TEMP-COL TO RR-TEMP-B
                 ADD 4 TO RR-TEMP-B-LEN
               END-IF

               IF RB-DTLS-LF(IDX) = 6
                  AND RR-TEMP-B(1:RR-TEMP-B-LEN) IS NUMERIC
                 MOVE "SEC." TO RR-TEMP-COL(1:4)
                 MOVE RR-TEMP-B(1:RR-TEMP-B-LEN)
                                         TO RR-TEMP-COL(6:RR-TEMP-B-LEN)
                 MOVE RR-TEMP-COL TO RR-TEMP-B
                 ADD 5 TO RR-TEMP-B-LEN
               END-IF

               MOVE RR-TEMP-A TO DTLS-LF(23)
               MOVE ", " TO DTLS-LF(23)(RR-TEMP-A-LEN + 1:2)
               MOVE RR-TEMP-B TO DTLS-LF(23)(RR-TEMP-A-LEN + 3:
                                             RR-TEMP-B-LEN)
             END-IF
           END-PERFORM.
           MOVE DTLS-LF(23)(3:
                LENGTH OF FUNCTION TRIM(DTLS-LF(23)) - 2)
                TO DTLS-LF(23).

      *******************************************************
      *> FA-ERROR-SECTION SECTION 處理錯誤資料
      *******************************************************
       FA-ERROR-SECTION.
           MOVE "N" TO RR-TEMP-FLAG.
           MOVE "PLEASE ENTER" TO RR-TEMP-COL.

           *> ZIP/ POST BOX 為空值
           IF DTLS-LF(1) = SPACES
             STRING 
               FUNCTION TRIM(RR-TEMP-COL) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " POSTAL CODE/ POST BOX" DELIMITED BY SIZE
               INTO RR-TEMP-COL
             END-STRING
             MOVE "Y" TO RR-TEMP-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.

           *> COUNTRY 為空值
           IF DTLS-LF(2) = SPACES
             STRING 
               FUNCTION TRIM(RR-TEMP-COL) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " COUNTRY" DELIMITED BY SIZE
               INTO RR-TEMP-COL
             END-STRING
             MOVE "Y" TO RR-TEMP-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.

           *> CITY 為空值 PROVINCE 皆為空值
           IF DTLS-LF(3) = SPACES  AND DTLS-LF(16) = SPACES
             STRING 
               FUNCTION TRIM(RR-TEMP-COL) DELIMITED BY SIZE
               COMMA-FLAG DELIMITED BY SPACES
               " CITY OR PROVINCE" DELIMITED BY SIZE
               INTO RR-TEMP-COL
             END-STRING
             MOVE "Y" TO RR-TEMP-FLAG
             MOVE "," TO COMMA-FLAG
           END-IF.

           *> OTHER 有值: PARSING FAILED. PLEASE CHECK INPUT
           IF DTLS-LF(24) NOT = SPACES
             MOVE "PARSING FAILED. PLEASE CHECK INPUT" TO RR-TEMP-COL
             MOVE "Y" TO RR-TEMP-FLAG
           END-IF.

           *> 錯誤分析: 若 TRIM(DTLS-LF(IDX)) 字數 > 35 -> ERROR
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
             IF LENGTH OF FUNCTION TRIM(DTLS-LF(IDX)) > 35
               MOVE "ADDRESS DATA IS TOO LONG" TO RR-TEMP-COL
             END-IF
           END-PERFORM

           *> 錯誤分析: 若包含特殊字體 -> ERROR
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
             LENGTH OF FUNCTION TRIM(ORIGIN-DATA)

             MOVE ORIGIN-DATA(IDX:1) TO WS-CH
             COMPUTE WS-CODE = FUNCTION ORD(WS-CH)
             IF WS-CODE < 32 OR WS-CODE > 126
               MOVE "Y" TO RR-TEMP-FLAG
               MOVE "CONTAINS INVALID CHARACTERS" TO RR-TEMP-COL
               EXIT PERFORM
             ELSE
               INSPECT ALLOWED-CH TALLYING WS-CODE FOR ALL WS-CH
               IF NOT((WS-CH >= "0" AND WS-CH <= "9") OR 
                  (WS-CH >= "A" AND WS-CH <= "Z") OR
                  (WS-CH >= "a" AND WS-CH <= "z") OR
                  WS-CH = SPACE OR
                  WS-CODE > 0)
                   MOVE "Y" TO RR-TEMP-FLAG
                   MOVE "CONTAINS INVALID CHARACTERS" TO RR-TEMP-COL
                   EXIT PERFORM
               END-IF
             END-IF
           END-PERFORM.

           IF RR-TEMP-FLAG = "Y"
             STRING 
               FUNCTION TRIM(RR-TEMP-COL) DELIMITED BY SIZE
               "." DELIMITED BY SIZE
               INTO RR-TEMP-COL
             END-STRING

             MOVE RR-TEMP-COL TO DTLS-LF(19)
           ELSE
             MOVE SPACES TO DTLS-LF(19)
           END-IF.

      *******************************************************
       FA-TRIM.
           MOVE      1                    TO    TD-S.
           MOVE      200                  TO    TD-LEN TD-L.
           *> 前端空白
           PERFORM VARYING TD-IDX FROM 1 BY 1 UNTIL TD-IDX > TD-LEN
             IF TD-TEMP(TD-IDX:1) = SPACES
               ADD   1                    TO     TD-S
               SUBTRACT 1                 FROM   TD-L
             ELSE
               EXIT PERFORM
             END-IF
           END-PERFORM.

           *> 後端空白
           PERFORM VARYING TD-IDX FROM TD-LEN BY -1 UNTIL TD-IDX = 0
             IF TD-TEMP(TD-IDX:1) = SPACES
               SUBTRACT 1                 FROM   TD-L
             ELSE
               EXIT PERFORM
             END-IF
           END-PERFORM.

           MOVE      TD-TEMP(TD-S:TD-L)   TO    TD-FINISH.

      *******************************************************
       FA-MERGE-RTN.
           MOVE SPACES TO MD-FINISH.
           IF MD-LEN-A = 0
             MOVE MD-STRING-A TO TD-TEMP
             PERFORM FA-TRIM
             MOVE TD-L        TO MD-LEN-A
             MOVE TD-FINISH   TO MD-STRING-A
           END-IF.

           IF MD-COL NOT = SPACES
             MOVE MD-COL TO MD-STRING-A(MD-LEN-A + 1:1)
             ADD 1 TO MD-LEN-A
           END-IF.

           IF MD-LEN-B = 0
             MOVE MD-STRING-B TO TD-TEMP
             PERFORM FA-TRIM
             MOVE TD-L        TO MD-LEN-B
             MOVE TD-FINISH   TO MD-STRING-B
           END-IF.

           IF MD-FLAG-N
             MOVE MD-STRING-A(1:MD-LEN-A) 
                                     TO MD-FINISH(1:MD-LEN-A)
             MOVE MD-STRING-B(1:MD-LEN-B)   
                                     TO MD-FINISH(MD-LEN-A + 1:MD-LEN-B)
             COMPUTE MD-L = MD-LEN-A + MD-LEN-B
           ELSE
             MOVE MD-STRING-A(1:MD-LEN-A) 
                                     TO MD-FINISH(1:MD-LEN-A)
             MOVE " "                TO MD-FINISH(MD-LEN-A + 1:1)
             MOVE MD-STRING-B(1:MD-LEN-B) 
                                     TO MD-FINISH(MD-LEN-A + 2:MD-LEN-B)
             COMPUTE MD-L = MD-LEN-A + 1 + MD-LEN-B
           END-IF.
           INITIALIZE MD-TEMP.

      *******************************************************
       FA-ZIP.
           *> ===== ===== =====  標準寫法  ===== ===== =====
           *> ===== =====  "XXX XXX" (有空白，分2段)  ===== =====
           MOVE "N" TO WK-TEMP-FLAG.
           EVALUATE DTLS-LF(2)
             *> 例: 芬蘭(FI) ZIP: "FI 99999"
             WHEN RR-TEMP-COL
               IF RR-NEXT-COL(1:PC-CNT-CHAR(IDX + 1)
                              + PC-CNT-NUM(IDX + 1)) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 加拿大
             *> *** ***，英數字混合而成的字串 固定"X9X 9X9"格式 例: H3Z 2Y7
             WHEN "CA"
               IF RR-TEMP-LEN = 3 AND RR-NEXT-LEN = 3 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC AND
                  RR-TEMP-COL(2:1) IS NUMERIC AND
                  RR-TEMP-COL(3:1) IS ALPHABETIC AND
                  RR-NEXT-COL(1:1) IS NUMERIC AND
                  RR-NEXT-COL(2:1) IS ALPHABETIC AND
                  RR-NEXT-COL(3:1) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 荷蘭
             *> 9999 XX 例：1071 DJ
             WHEN "NL"
               IF RR-TEMP-LEN = 4 AND PC-CNT-NUM(IDX) = 4 AND
                  RR-NEXT-LEN = 2 AND PC-CNT-CHAR(IDX + 1) = 2
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 英國(GB)/ 耿西(GG)/ 澤西(JE) / 曼島(IM)
             *> 前半: X9/ X99/ X9X/ XX9/ XX99/ XX9X
             *> 後半: 9XX
             WHEN "GB"
             WHEN "GG"
             WHEN "JE"
             WHEN "IM"
               IF (RR-TEMP-LEN >= 2 AND RR-TEMP-LEN <= 4 AND
                   PC-CNT-NUM(IDX) > 0 AND PC-CNT-CHAR(IDX) > 0 AND
                   RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER) AND
                  (RR-NEXT-LEN = 3 AND
                   PC-CNT-NUM(IDX + 1) = 1 AND PC-CNT-CHAR(IDX + 1) = 2
                   AND RR-NEXT-COL(1:1) IS NUMERIC
                   AND RR-NEXT-COL(2:2) IS ALPHABETIC-UPPER)
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 馬爾他
             *> 前半: XXX
             *> 後半: 9999
             WHEN "MT"
               IF RR-TEMP-LEN = 3 AND PC-CNT-CHAR(IDX) = 3 AND
                  RR-NEXT-LEN = 4 AND PC-CNT-NUM(IDX + 1) = 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 愛爾蘭
             *> 前後段字數總和7 且前段首碼為英文字母
             WHEN "IE"
               IF RR-TEMP-LEN + RR-NEXT-LEN = 7 AND
                  PC-CNT-NUM(IDX) + PC-CNT-NUM(IDX + 1) +
                  PC-CNT-CHAR(IDX) + PC-CNT-CHAR(IDX + 1) = 7 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 百慕達
             *> 前半: XX
             *> 後半: 99
             WHEN "BM"
               IF RR-TEMP-LEN = 2 AND PC-CNT-CHAR(IDX) = 2 AND
                  RR-NEXT-LEN = 2 AND PC-CNT-NUM(IDX + 1) = 2
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 美屬維京群島
             *> 前半: XX
             *> 後半: 99999
             WHEN "VI"
               IF RR-TEMP-LEN = 2 AND PC-CNT-CHAR(IDX) = 2 AND
                  RR-NEXT-LEN = 5 AND PC-CNT-NUM(IDX + 1) = 5
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 盧森堡
             *> 前半: L(固定)
             *> 後半: 9999
             WHEN "LU"
               IF RR-TEMP-COL = "L" AND
                  RR-NEXT-LEN = 4 AND PC-CNT-NUM(IDX + 1) = 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 史瓦帝尼
             *> X 999
             WHEN "SZ"
               IF PC-CNT-CHAR(IDX) = 1 AND PC-CNT-NUM(IDX + 1) >= 3 AND
                  RR-TEMP-COL IS ALPHABETIC-UPPER AND
                  RR-NEXT-COL IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 汶萊
             WHEN "BN"
               IF RR-TEMP-LEN = 2 AND PC-CNT-CHAR(IDX) = 2 AND
                  RR-NEXT-LEN = 4 AND PC-CNT-NUM(IDX + 1) = 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 匈牙利
             WHEN "HU"
               IF RR-TEMP-COL = "H" AND PC-CNT-NUM(IDX + 1) >= 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 牙買加
             *> CITYNAME 9or99
             WHEN "JM"
               IF PC-CNT-NUM(IDX) > 0
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 聖赫勒拿
             WHEN "SH"
               IF RR-TEMP-COL = "STHL" AND
                  RR-NEXT-COL = "1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 特克斯與凱科斯群島
             WHEN "TC"
               IF RR-TEMP-COL = "TKCA" AND
                  RR-NEXT-COL = "1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 福克蘭群島
             WHEN "FK"
               IF RR-TEMP-COL = "FIQQ" AND
                  RR-NEXT-COL = "1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
           END-EVALUATE.

           PERFORM VARYING KDX FROM 1 BY 1 UNTIL KDX > 35 OR
                                                 WK-TEMP-FLAG = "Y"
             IF LS-LIST-COL(1 KDX) = SPACES
               EXIT PERFORM
             END-IF

             IF RR-TEMP-COL = LS-LIST-COL(1 KDX) AND
                PC-CNT-NUM(IDX + 1) = RR-NEXT-LEN
                  MOVE "Y" TO WK-TEMP-FLAG             
             END-IF
           END-PERFORM.

           *> 若判斷為標準寫法 加入後欄位並離開 FA-ZIP
           IF WK-TEMP-FLAG = "Y"
             MOVE "Y" TO RR-TEMP-FLAG RR-NEXT-FLAG
             MOVE 1   TO RR-DTLS-FLAG
             MOVE "," TO RR-PRE-FLAG
             EXIT PARAGRAPH
           END-IF.

           *> ===== ===== =====  手寫常見  ===== ===== =====
           *> ===== =====  "XXXXXX"  (無空白，僅1段)  ===== =====
           EVALUATE DTLS-LF(2)
             *> 例: 芬蘭(FI) ZIP: "FI 99999"
             WHEN RR-TEMP-COL(1:2)
               IF (RR-TEMP-COL(3:1) = "-" AND
                   RR-TEMP-COL(4:RR-TEMP-LEN - 3) IS NUMERIC) OR
                  (RR-TEMP-COL(3:RR-TEMP-LEN - 2) IS NUMERIC)
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 加拿大
             *> *** ***，英數字混合而成的字串 固定"X9X 9X9"格式 例: H3Z 2Y7
             WHEN "CA"
               IF PC-CNT-NUM(IDX) >= 3 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC AND
                  RR-TEMP-COL(2:1) IS NUMERIC AND
                  RR-TEMP-COL(3:1) IS ALPHABETIC AND
                  RR-TEMP-COL(4:1) IS NUMERIC AND
                  RR-TEMP-COL(5:1) IS ALPHABETIC AND
                  RR-TEMP-COL(6:1) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 荷蘭
             *> 9999XX 例：1071DJ
             WHEN "NL"
               IF RR-TEMP-COL(1:4) IS NUMERIC AND
                  RR-TEMP-COL(RR-TEMP-LEN - 1:2) IS ALPHABETIC AND
                  PC-CNT-NUM(IDX) >= 4 AND PC-CNT-CHAR(IDX) = 2
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 英國
             *> 前半: X9/ X99/ X9X/ XX9/ XX99/ XX9X
             *> 後半: 9XX
             WHEN "GB"
             WHEN "GG"
             WHEN "JE"
             WHEN "IM"
               IF RR-TEMP-LEN >= 5 AND RR-TEMP-LEN <= 8 AND
                   PC-CNT-NUM(IDX) > 1 AND PC-CNT-CHAR(IDX) > 3 AND
                   RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER AND
                   RR-TEMP-COL(RR-TEMP-LEN - 2:1) IS NUMERIC AND
                   RR-TEMP-COL(RR-TEMP-LEN - 1:2) IS ALPHABETIC-UPPER
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 馬爾他
             *> XXX9999
             WHEN "MT"
               IF RR-TEMP-COL(1:3) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(RR-TEMP-LEN - 3:4) IS NUMERIC AND
                  PC-CNT-CHAR(IDX) = 3 AND PC-CNT-NUM(IDX) >= 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 愛爾蘭
             *> 前後段字數總和7 且前段首碼為英文字母
             WHEN "IE"
               IF RR-TEMP-LEN = 7 AND
                  PC-CNT-NUM(IDX) + PC-CNT-CHAR(IDX) >= 7 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 百慕達
             *> XX99
             WHEN "BM"
               IF PC-CNT-CHAR(IDX) = 2 AND PC-CNT-NUM(IDX) >= 2 AND
                  RR-TEMP-COL(1:2) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(RR-TEMP-LEN - 1:2) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 美屬維京群島
             *> XX99999
             WHEN "VI"
               IF PC-CNT-CHAR(IDX) = 2 AND PC-CNT-NUM(IDX) >= 5 AND
                  RR-TEMP-COL(1:2) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(RR-TEMP-LEN - 4:2) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 盧森堡
             *> L(固定)9999
             WHEN "LU"
               IF PC-CNT-CHAR(IDX) = 1 AND PC-CNT-NUM(IDX) >= 4 AND
                  RR-TEMP-COL(1:1) = "L" AND
                  RR-TEMP-COL(RR-TEMP-LEN - 3:4) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 史瓦帝尼
             *> X999
             WHEN "SZ"
               IF PC-CNT-CHAR(IDX) = 1 AND PC-CNT-NUM(IDX) >= 3 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(RR-TEMP-LEN - 2:3) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 匈牙利
             WHEN "HU"
               IF RR-TEMP-COL(1:1) = "H" AND
                  RR-TEMP-COL(RR-TEMP-LEN - 3:4) IS NUMERIC AND
                  PC-CNT-NUM(IDX) >= 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 瑙魯
             WHEN "NR"
               IF RR-TEMP-COL = "NRU68"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 汶萊
             WHEN "BN"
               IF RR-TEMP-LEN >= 6 AND PC-CNT-CHAR(IDX) = 2 AND
                  PC-CNT-NUM(IDX) = 4
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 聖赫勒拿
             WHEN "SH"
               IF RR-TEMP-COL = "STHL1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 特克斯與凱科斯群島
             WHEN "TC"
               IF RR-TEMP-COL = "TKCA1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 福克蘭群島
             WHEN "FK"
               IF RR-TEMP-COL = "FIQQ1ZZ"
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 哈薩克
             WHEN "KZ"
               IF PC-CNT-CHAR(IDX) > 0 AND PC-CNT-NUM(IDX) > 0 AND
                  PC-CNT-CHAR(IDX) + PC-CNT-NUM(IDX) >= 7
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 阿根廷
             WHEN "AR"
               IF PC-CNT-CHAR(IDX) + PC-CNT-NUM(IDX) >= 8 AND
                  RR-TEMP-COL(1:1) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(2:4) IS NUMERIC AND
                  RR-TEMP-COL(6:3) IS ALPHABETIC-UPPER
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 迦納
             WHEN "GH"
               IF PC-CNT-CHAR(IDX) = 2 AND + PC-CNT-NUM(IDX) >= 7 AND
                  RR-TEMP-COL(1:2) IS ALPHABETIC-UPPER AND
                  RR-TEMP-COL(RR-TEMP-LEN - 3:4) IS NUMERIC
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
             *> 孟加拉
             *> CITY-9999
             WHEN "BD"
               IF PC-CNT-NUM(IDX) >= 4 AND
                  RR-TEMP-COL(RR-TEMP-LEN - 4:1) = "-" AND
                  RR-TEMP-COL(RR-TEMP-LEN - 3:4) IS NUMERIC
                 MOVE RR-TEMP-COL(1:RR-TEMP-LEN - 5) TO DTLS-LF(3)
                 MOVE IDX TO PC-OTHER-CITY
                 MOVE RR-TEMP-COL(RR-TEMP-LEN - 3:4) TO RR-TEMP-COL
                 MOVE "Y" TO WK-TEMP-FLAG
               END-IF
           END-EVALUATE.

           IF WK-TEMP-FLAG = "Y"
             MOVE "Y" TO RR-TEMP-FLAG
             MOVE 1 TO RR-DTLS-FLAG
             MOVE "," TO RR-PRE-FLAG
           END-IF.

      *******************************************************
      *> 結束處理
      *******************************************************
      *    EXIT PROGRAM.
      *END PROGRAM FORMATTER-ADDRESS.
