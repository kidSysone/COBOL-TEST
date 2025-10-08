       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-ADDRESS-CSV.

       ENVIRONMENT DIVISION.
      *******************************************************
      *> 環境部
      *> 檔案名稱：Address_Split.txt
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）

      *> 檔案名稱：Fail_Data.txt
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）
      *******************************************************
       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT OUT-FILE ASSIGN 
             TO 'output\Address_Split.csv'
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE ASSIGN
             TO 'output\Fail_Data.csv'
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *******************************************************
      *> 資料部、檔案定義區段（輸出檔案）
      *> 記錄結構：2000位元組的字串
      *******************************************************
       FILE SECTION.
       FD OUT-FILE.
         01 OUT-FILE-REC PIC X(2000).

       FD ERROR-FILE.
         01 ERROR-REC PIC X(2000).

      *******************************************************
      *> 資料部、LOCAL-STORAGE SECTION
      *******************************************************
       LOCAL-STORAGE SECTION.
       *> === 標題 ===
       01 TITLE-ADDRESS-DATA.
              05 WS-COL-TEXT   PIC X(50) OCCURS 21 TIMES.

       01 IDX                  PIC 9999.
       01 JDX                  PIC 99.
       01 TMP-DATA             PIC X(500).
       01 TMP-REC              PIC X(2000).

       *> === TOTAL ===
       01 TOTAL-DATA.
              05 DATA-COUNT    PIC 9999.
              05 DATA-FMT      PIC ZZZ9.
              05 TOTAL-COMMENT PIC X(2000).

       *> === ERROR ===
       01 ERROR-DATA.
              05 ERROR-COUNT   PIC 9999.
              05 ERROR-FMT     PIC ZZZ9.

      *******************************************************
      *> 資料部、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01  ADDRESS-DATA.
           03 ADDRESS-COLUMNS OCCURS 1000 TIMES.
              05  ADDRESS-FULLNAME PIC X(2000).
              05  ADDRESS-DTLS     PIC X(100) OCCURS 20 TIMES.
              05  ZIP              PIC X(35).  *> 1 郵遞區號
              05  COUNTRY          PIC X(100). *> 2 國家
              05  CITY             PIC X(100). *> 3 縣市
              05  DISTRICT         PIC X(100). *> 4 市區
              05  STREET           PIC X(100). *> 5 路
              05  SEC              PIC X(35).  *> 6 段
              05  LANE             PIC X(50).  *> 7 巷
              05  ALLEY            PIC X(35).  *> 8 弄
              05  M-NO             PIC X(35).  *> 9 號
              05  S-NO             PIC X(35).  *> 10 號
              05  M-FLOOR          PIC X(35).  *> 11 樓
              05  S-FLOOR          PIC X(35).  *> 12 樓
              05  ROOM             PIC X(35).  *> 13 室
              05  BUILDING         PIC X(100). *> 14 建築大樓
              05  VILLAGE          PIC X(100). *> 15 村里
              05  PROVINCE         PIC X(35).  *> 16 省份
              05  STATE            PIC X(100). *> 17 州
              05  OTHER-COL        PIC X(100). *> 18 其他

              05  ERROR-COMMENT    PIC X(40).  *> 19 錯誤
              05  CUSTOMER_ID      PIC X(15).  *> 客戶 ID


      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING ADDRESS-DATA.

      *******************************************************
      *> 輸出: Address_Split.txt
      *******************************************************
              MOVE "ZIP"      TO WS-COL-TEXT(1).
              MOVE "COUNTRY"  TO WS-COL-TEXT(2).
              MOVE "CITY"     TO WS-COL-TEXT(3).
              MOVE "DISTRICT" TO WS-COL-TEXT(4).
              MOVE "STREET"   TO WS-COL-TEXT(5).
              MOVE "SEC"      TO WS-COL-TEXT(6).
              MOVE "LANE"     TO WS-COL-TEXT(7).
              MOVE "ALLEY"    TO WS-COL-TEXT(8).
              MOVE "NUMBER"   TO WS-COL-TEXT(9).
              MOVE "NUMBER"   TO WS-COL-TEXT(10).
              MOVE "FLOOR"    TO WS-COL-TEXT(11).
              MOVE "FLOOR"    TO WS-COL-TEXT(12).
              MOVE "ROOM"     TO WS-COL-TEXT(13).
              MOVE "BUILDING" TO WS-COL-TEXT(14).
              MOVE "VILLAGE"  TO WS-COL-TEXT(15).
              MOVE "PROVINCE" TO WS-COL-TEXT(16).
              MOVE "STATE"    TO WS-COL-TEXT(17).
              MOVE "OTHER"    TO WS-COL-TEXT(18).
              MOVE "ERROR_MESSAGE"    TO WS-COL-TEXT(19).
              MOVE "CUSTOMER_ID"      TO WS-COL-TEXT(20).
              MOVE "ADDRESS_NAME"     TO WS-COL-TEXT(21).


           *>   ============  標題 ============
           STRING
             FUNCTION TRIM(WS-COL-TEXT(20)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(WS-COL-TEXT(21)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC
           END-STRING.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
             IF IDX NOT = 10 AND IDX NOT = 12 *> SUB號碼/樓層略過
               STRING
                 FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                 FUNCTION TRIM(WS-COL-TEXT(IDX)) DELIMITED BY SIZE
                 ";" DELIMITED BY SIZE
                 INTO TMP-REC
               END-STRING
             END-IF
           END-PERFORM.

       *> 開啟檔案、OUT-FILE
           OPEN OUTPUT OUT-FILE

           *> === 輸出標題 ===
           MOVE TMP-REC(1:LENGTH OF FUNCTION TRIM(TMP-REC) - 1) 
             TO OUT-FILE-REC
           WRITE OUT-FILE-REC

           *> === 輸出內容 ===
           *> IDX: 記錄數迴圈，最多1000行
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 1000
           IF ADDRESS-FULLNAME(IDX)  NOT = SPACES
                 IF ERROR-COMMENT(IDX) NOT = SPACES
                   ADD 1 TO ERROR-COUNT
                 END-IF

                 ADD 1 TO DATA-COUNT
                 *> 將19個欄位合併為1行寫入
                 MOVE SPACES TO OUT-FILE-REC TMP-REC

                   *> CUSTOMER_ID、ADDRESS_NAME
                   STRING
                     FUNCTION TRIM(CUSTOMER_ID(IDX)) DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     FUNCTION TRIM(ADDRESS-FULLNAME(IDX))
                       DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                 *> JDK: 資料欄位
                 PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > 19
                 IF JDX NOT = 10 AND JDX NOT = 12 *> SUB號碼/樓層略過

                   MOVE FUNCTION TRIM(ADDRESS-DTLS(IDX JDX)) TO TMP-DATA
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     FUNCTION TRIM(ADDRESS-DTLS(IDX JDX))
                       DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING
                 END-IF
                 END-PERFORM

                 *> 寫入檔案
                 MOVE TMP-REC(1:LENGTH OF FUNCTION TRIM(TMP-REC) - 1) 
                   TO OUT-FILE-REC
                 WRITE OUT-FILE-REC
           ELSE
             EXIT PERFORM
           END-IF
           END-PERFORM.


           *> 總筆數
           MOVE ERROR-COUNT TO ERROR-FMT.
           MOVE DATA-COUNT  TO DATA-FMT.
           STRING
             "RESULT SUMMARY->;TOTAL ITEMS: " DELIMITED BY SIZE
             DATA-FMT DELIMITED BY SIZE
             ", ERROR ITEMS: " DELIMITED BY SIZE
             ERROR-FMT DELIMITED BY SIZE
             INTO TOTAL-COMMENT
           END-STRING.

           *> 寫入檔案
           MOVE TOTAL-COMMENT TO OUT-FILE-REC.
           WRITE OUT-FILE-REC.


       *> 關閉檔案
           CLOSE OUT-FILE


      *******************************************************
      *> 輸出: Fail_Data.txt
      *******************************************************
           *> 標題設定
              MOVE "CUSTOMER_ID"    TO WS-COL-TEXT(1).
              MOVE "ADDRESS_NAME"   TO WS-COL-TEXT(2).
              MOVE "ERROR_MESSAGE"  TO WS-COL-TEXT(3).
              MOVE SPACES TO ERROR-REC TMP-REC.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                   FUNCTION TRIM(WS-COL-TEXT(IDX)) DELIMITED BY SIZE
                   ";" DELIMITED BY SIZE
                   INTO TMP-REC
                 END-STRING
           END-PERFORM

       *> 開啟檔案、ERROR-FILE
           OPEN OUTPUT ERROR-FILE

           *> === 輸出標題 ===
           MOVE TMP-REC(1:LENGTH OF FUNCTION TRIM(TMP-REC) - 1) 
             TO ERROR-REC
           WRITE ERROR-REC


           *> ===============       有錯誤資料       ===============
           IF ERROR-COUNT NOT = 0
           *> IDX: 記錄數迴圈，最多1000行
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 1000
           IF ADDRESS-FULLNAME(IDX)  NOT = SPACES
             IF ERROR-COMMENT(IDX) NOT = SPACES
                 *> 將3個欄位合併為1行寫入
                 MOVE SPACES TO ERROR-REC TMP-REC

                   *> CUSTOMER_ID、ADDRESS_NAME
                   STRING
                     ";" DELIMITED BY SIZE
                     FUNCTION TRIM(CUSTOMER_ID(IDX)) DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     FUNCTION TRIM(ADDRESS-FULLNAME(IDX))
                       DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     FUNCTION TRIM(ERROR-COMMENT(IDX)) DELIMITED BY SIZE
                     ";" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                 *> 寫入檔案
                 MOVE TMP-REC(2:LENGTH OF FUNCTION TRIM(TMP-REC) - 2) 
                   TO ERROR-REC
                 WRITE ERROR-REC
             END-IF
           ELSE
             EXIT PERFORM
           END-IF
           END-PERFORM


             *> 總筆數
             MOVE SPACES TO TMP-REC
             STRING
               "RESULT SUMMARY->;ERROR ITEMS: " DELIMITED BY SIZE
               ERROR-FMT DELIMITED BY SIZE
               INTO TMP-REC
             END-STRING

             *> 寫入檔案
             MOVE TMP-REC TO ERROR-REC
             WRITE ERROR-REC

           *> ===============       無錯誤資料       ===============
           ELSE
             MOVE "RESULT SUMMARY->;NO ERROR DATA;" TO TMP-REC
             MOVE TMP-REC TO ERROR-REC
             WRITE ERROR-REC
           END-IF.

       *> 關閉檔案
           CLOSE ERROR-FILE

           EXIT PROGRAM.
       END PROGRAM OUTPUT-ADDRESS-CSV.
