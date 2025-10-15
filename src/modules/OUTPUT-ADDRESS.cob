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
           05 WS-COL-TEXT  PIC X(50) OCCURS 25 TIMES.
           05 WS-COL-LEN   PIC 9(3) OCCURS 25 TIMES.

           05 TMP-TOTAL-TXT       PIC X(2000).
           05 TMP-ERROR-TXT       PIC X(2000).
           05 WS-COL-TEXT-ERROR   PIC X(50) OCCURS 25 TIMES.
           05 WS-COL-LEN-ERROR    PIC 9(3)  OCCURS 4 TIMES.

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-FORMATTER LS-OUTPUT.

      *******************************************************
      *> TMP-TOTAL  ->  OUT-FILE-REC-CSV
      *******************************************************
           *> 將19個欄位合併為1行寫入
           MOVE SPACES TO TMP-REC TMP-ERROR.

           *> CUSTOMER_ID、ADDR_LINE_ORIG、ADDR_LINE_EN
           STRING
             FUNCTION TRIM(DTLS-LF(20)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(DTLS-LF(21)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(DTLS-LF(22)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC
           END-STRING.

           *> JDK: 資料欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
           IF IDX NOT = 10 AND IDX NOT = 12 *> SUB號碼/樓層略過

             MOVE FUNCTION TRIM(DTLS-LF(IDX)) TO TMP-DATA
             STRING
               FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
               FUNCTION TRIM(DTLS-LF(IDX)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               INTO TMP-REC
             END-STRING
           END-IF
           END-PERFORM.

           *> REBUILD
           STRING
             FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
             FUNCTION TRIM(DTLS-LF(23)) DELIMITED BY SIZE
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
           IF DTLS-LF(19) NOT = SPACES

             *> 將4個欄位合併為1行寫入
             MOVE SPACES TO TMP-ERROR

            *> CUSTOMER_ID、ADDRESS_NAME
             STRING
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(DTLS-LF(20)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(DTLS-LF(21)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(DTLS-LF(22)) DELIMITED BY SIZE
               ";" DELIMITED BY SIZE
               FUNCTION TRIM(DTLS-LF(19)) DELIMITED BY SIZE
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

           *> CUSTOMER_ID
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             DTLS-LF(20)(1:WS-COL-LEN(20))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.

           *> ADDR_LINE_ORIG
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             DTLS-LF(21)(1:WS-COL-LEN(21))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.
            
           *> ADDR_LINE_EN
           STRING
             FUNCTION TRIM(TMP-TOTAL-TXT) DELIMITED BY SIZE
             DTLS-LF(22)(1:WS-COL-LEN(22))
               DELIMITED BY SIZE
             " |" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.

           *> JDK: 資料欄位
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
           IF IDX NOT = 10 AND IDX NOT = 12 *> SUB號碼/樓層略過

             MOVE FUNCTION TRIM(DTLS-LF(IDX)) TO TMP-DATA
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
             DTLS-LF(23)(1:WS-COL-LEN(23))
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
           IF DTLS-LF(19) NOT = SPACES 
              *> 將3個欄位合併為1行寫入
              MOVE "|" TO TMP-ERROR-TXT

              *> CUSTOMER_ID
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                DTLS-LF(20)(1:WS-COL-LEN-ERROR(1))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ADDRESS_LINE_ORIGIN
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                DTLS-LF(21)(1:WS-COL-LEN-ERROR(2))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ADDRESS_LINE_EN
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                DTLS-LF(22)(1:WS-COL-LEN-ERROR(3))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING

              *> ERROR_MESSAGE
              STRING
                FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
                DTLS-LF(19)(1:WS-COL-LEN-ERROR(4))
                  DELIMITED BY SIZE
                " |" DELIMITED BY SIZE
                INTO TMP-ERROR-TXT
              END-STRING
           END-IF.

           EXIT PROGRAM.
       END PROGRAM OUTPUT-ADDRESS.
