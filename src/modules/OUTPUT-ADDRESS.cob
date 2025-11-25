       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-ADDRESS.

       DATA DIVISION.
      *******************************************************
      *> 資料部、LOCAL-STORAGE SECTION
      *******************************************************
       LOCAL-STORAGE SECTION.
       01 IDX                  PIC 9999.
       01 TMP-DATA             PIC X(500).
       01 TMP-REC              PIC X(2000).
       01 WS-FIELD-LEN  PIC 999  VALUE 35.   *> 欄位寬度
       01 OP-LIST              PIC X(500) OCCURS 25 TIMES.   *> 輸出欄位
       01 TEMP-A                PIC X(500).
       01 TEMP-B                PIC X(500).


      *******************************************************
      *> 資料部、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-FORMATTER.
           05 BEFORE-DATA  PIC X(500). *> 格式化讀取資料
           05 AFTER-DATA   PIC X(500). *> 格式化回傳資料
           05 ORIGIN-DATA  PIC X(500). *> 原文
           05 DTLS-LF      PIC X(500) OCCURS 25 TIMES. *> 地址欄位

       01 LS-OUTPUT.
           05 TMP-TOTAL    PIC X(2000).
           05 TMP-ERROR    PIC X(2000).
           05 WS-COL-TEXT  PIC X(50) OCCURS 25 TIMES. *> 欄首名稱
           05 WS-COL-LEN   PIC 9(3) OCCURS 25 TIMES.  *> 欄位寬度

           05 TMP-TOTAL-TXT       PIC X(2000).
           05 TMP-ERROR-TXT       PIC X(2000).
           05 WS-COL-TEXT-ERROR   PIC X(50) OCCURS 25 TIMES. *> 欄首名稱
           05 WS-COL-LEN-ERROR    PIC 9(3)  OCCURS 4 TIMES.  *> 欄位寬度

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-FORMATTER LS-OUTPUT.

      *******************************************************
      *> INITIALIZATION SECTION 初始化
      *******************************************************
       INITIALIZATION SECTION.
           *> 初始化 OP-LIST
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 25
             MOVE SPACES TO OP-LIST(IDX)
           END-PERFORM.

           MOVE DTLS-LF(1)  TO OP-LIST(9).   *> 9  POST_CODE
           MOVE DTLS-LF(2)  TO OP-LIST(14).  *> 14 COUNTRY
           MOVE DTLS-LF(3)  TO OP-LIST(11).  *> 11 TOWN_LOCATION_NAME
           MOVE DTLS-LF(4)  TO OP-LIST(12).  *> 12 DISTRICT_NAME

      *>   8[ALLEY] → 7[LANE] → 6[SEC] → 5[SREET]
           MOVE SPACES TO TMP-DATA.
           PERFORM VARYING IDX FROM 8 BY -1 UNTIL IDX = 4
             IF DTLS-LF(IDX) NOT = SPACES
               MOVE FUNCTION TRIM(TMP-DATA) TO TEMP-A
               MOVE FUNCTION TRIM(DTLS-LF(IDX)) TO TEMP-B
               MOVE SPACES TO TMP-DATA

               IF IDX = 8 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Aly. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TMP-DATA
                 END-STRING
                 MOVE TMP-DATA TO TEMP-B
               END-IF

               IF IDX = 7 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Ln. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TMP-DATA
                 END-STRING
                 MOVE TMP-DATA TO TEMP-B
               END-IF

               IF IDX = 6 AND FUNCTION TRIM(TEMP-B) IS NUMERIC
                 STRING
                   "Sec. " DELIMITED BY SIZE
                   FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                   INTO TMP-DATA
                 END-STRING
                 MOVE TMP-DATA TO TEMP-B
               END-IF

               STRING
                 FUNCTION TRIM(TEMP-A) DELIMITED BY SIZE
                 ", " DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-B) DELIMITED BY SIZE
                 INTO TMP-DATA
               END-STRING
             END-IF
           END-PERFORM.

           MOVE FUNCTION TRIM(
                      TMP-DATA(2:LENGTH OF FUNCTION TRIM(TMP-DATA) - 1))
                            TO OP-LIST(3).   *> 3  STREET_NAME
           MOVE DTLS-LF(9)  TO OP-LIST(4).   *> 4  BUILDING_NUMBER
           MOVE DTLS-LF(10) TO OP-LIST(1).   *> 1  DEPARTMENT
           MOVE DTLS-LF(11) TO OP-LIST(6).   *> 6  FLOOR
           MOVE DTLS-LF(12) TO OP-LIST(7).   *> 7  POST_BOX
           MOVE DTLS-LF(13) TO OP-LIST(8).   *> 8  ROOM
           MOVE DTLS-LF(14) TO OP-LIST(5).   *> 5  BUILDING_NAME
           MOVE DTLS-LF(15) TO OP-LIST(10).  *> 10 TOWN_NAME

           MOVE DTLS-LF(16) TO OP-LIST(13).
           IF DTLS-LF(17) NOT = SPACES
             MOVE DTLS-LF(17) TO OP-LIST(13) *> 13 COUNTRY_SUB_DIVISION
           END-IF

           MOVE DTLS-LF(18) TO OP-LIST(2).   *>  2 SUB_DEPARTMENT
           MOVE DTLS-LF(19) TO OP-LIST(19).
           MOVE DTLS-LF(20) TO OP-LIST(20).
           MOVE DTLS-LF(21) TO OP-LIST(21).
           MOVE DTLS-LF(22) TO OP-LIST(22).
           MOVE DTLS-LF(23) TO OP-LIST(23).
           MOVE DTLS-LF(24) TO OP-LIST(18).


      *******************************************************
      *> MAIN SECTION 主要程序
      *******************************************************
       MAIN SECTION.
      *******************************************************
      *> TMP-TOTAL  ->  OUT-FILE-REC-CSV
      *******************************************************
           *> 將19個欄位合併為1行寫入
           MOVE SPACES TO TMP-REC TMP-ERROR.

           *> CIFKEY、ADDR_LINE_ORIG、ADDR_LINE_EN
           STRING
             FUNCTION TRIM(DTLS-LF(20)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(OP-LIST(21)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(OP-LIST(22)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC
           END-STRING.

           *> JDK: 資料欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
           IF IDX NOT = 16 AND IDX NOT = 17 *> 16/17略過
             MOVE FUNCTION TRIM(OP-LIST(IDX)) TO TMP-DATA
             STRING
               FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
               FUNCTION TRIM(OP-LIST(IDX)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               INTO TMP-REC
             END-STRING
           END-IF
           END-PERFORM.

           *> REBUILD
           STRING
             FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
             FUNCTION TRIM(OP-LIST(23)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC
           END-STRING.

           *> 寫入檔案
           MOVE TMP-REC(1:LENGTH OF FUNCTION TRIM(TMP-REC) - 1) 
             TO TMP-TOTAL.

      *******************************************************
      *> TMP-ERROR  ->  ERROR-FILE-CSV
      *******************************************************
           *> 有錯誤訊息，寫入 ERROR-FILE-CSV
           IF OP-LIST(19) NOT = SPACES

             *> 將4個欄位合併為1行寫入
             MOVE SPACES TO TMP-ERROR

            *> CIFKEY、ADDRESS_NAME
             STRING
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(OP-LIST(20)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(OP-LIST(21)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(OP-LIST(22)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(OP-LIST(19)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               INTO TMP-ERROR
             END-STRING
           END-IF.
           MOVE TMP-ERROR(2:LENGTH OF FUNCTION TRIM(TMP-ERROR) - 2)
             TO TMP-ERROR.


      *******************************************************
      *> 輸出: Address_Split.txt
      *******************************************************

           *> 將19個欄位合併為1行寫入
           MOVE "|" TO TMP-TOTAL-TXT.

           *> CIFKEY
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             OP-LIST(20)(1:WS-COL-LEN(20))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.

           *> ADDR_LINE_ORIG
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             OP-LIST(21)(1:WS-COL-LEN(21))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.
            
           *> ADDR_LINE_EN
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             OP-LIST(22)(1:WS-COL-LEN(22))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.

           *> JDK: 資料欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
           IF IDX NOT = 16 AND IDX NOT = 17 *> 16/17略過
             MOVE FUNCTION TRIM(OP-LIST(IDX)) TO TMP-DATA
             MOVE WS-COL-LEN(IDX) TO WS-FIELD-LEN

             STRING
               FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
               TMP-DATA(1:WS-FIELD-LEN)
                 DELIMITED BY SIZE
               " |" DELIMITED BY SIZE
               INTO TMP-TOTAL-TXT
             END-STRING
           END-IF
           END-PERFORM.

           *> ADDR_LINE_REBUILD
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             OP-LIST(23)(1:WS-COL-LEN(23))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.


      *******************************************************
      *> 輸出: Fail_Data.txt
      *******************************************************
           *> === 輸出內容 ===
           COMPUTE WS-FIELD-LEN 
             = WS-COL-LEN-ERROR(1) + WS-COL-LEN-ERROR(2) + 
               WS-COL-LEN-ERROR(3) + WS-COL-LEN-ERROR(4) +
               2 * 3 + 1.

           *> ===============       有錯誤資料       ===============
           MOVE SPACES TO TMP-ERROR-TXT.
           IF OP-LIST(19) NOT = SPACES 
              *> 將3個欄位合併為1行寫入
              MOVE "|" TO TMP-ERROR-TXT

              *> CIFKEY
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                OP-LIST(20)(1:WS-COL-LEN-ERROR(1))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ADDRESS_LINE_ORIGIN
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                OP-LIST(21)(1:WS-COL-LEN-ERROR(2))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ADDRESS_LINE_EN
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                OP-LIST(22)(1:WS-COL-LEN-ERROR(3))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ERROR_MESSAGE
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                OP-LIST(19)(1:WS-COL-LEN-ERROR(4))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING
           END-IF.

           EXIT PROGRAM.
       END PROGRAM OUTPUT-ADDRESS.
