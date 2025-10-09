       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-ADDRESS.

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
             TO 'output\Address_Split.txt'
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE ASSIGN
             TO 'output\Fail_Data.txt'
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
              05 WS-FIELD-LEN  PIC 999  VALUE 35.   *> 欄位寬度
              05 WS-DATA-LEN   PIC 999.             *> 資料長度
              05 WS-LEFT-PAD   PIC 999.             *> 左邊空白數
              05 WS-RIGHT-PAD  PIC 999.             *> 右邊空白數
              05 WS-CENTER-FLD PIC X(1000).         *> 輸出欄位

              05 WS-COL-LEN    PIC 999   OCCURS 21 TIMES.
              05 WS-COL-TEXT   PIC X(50) OCCURS 21 TIMES.
              05 DIVIDING-LINE PIC X(2000).

       01 IDX                  PIC 9999.
       01 JDX                  PIC 99.
       01 TMP-DATA             PIC X(500).
       01 TMP-REC              PIC X(2000).

       *> === TOTAL ===
       01 TOTAL-DATA.
              05 TOTAL-LEN     PIC 9999 VALUE 0.
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
           *> 標題設定
              MOVE 35 TO WS-COL-LEN(1).
              MOVE 35 TO WS-COL-LEN(2).
              MOVE 100 TO WS-COL-LEN(3).
              MOVE 100 TO WS-COL-LEN(4).
              MOVE 50 TO WS-COL-LEN(5).
              MOVE 35 TO WS-COL-LEN(6).
              MOVE 50 TO WS-COL-LEN(7).
              MOVE 35 TO WS-COL-LEN(8).
              MOVE 35 TO WS-COL-LEN(9).
              MOVE 35 TO WS-COL-LEN(10).
              MOVE 35 TO WS-COL-LEN(11).
              MOVE 35 TO WS-COL-LEN(12).
              MOVE 35 TO WS-COL-LEN(13).
              MOVE 35 TO WS-COL-LEN(14).
              MOVE 35 TO WS-COL-LEN(15).
              MOVE 35 TO WS-COL-LEN(16).
              MOVE 10 TO WS-COL-LEN(17).
              MOVE 35 TO WS-COL-LEN(18).
              MOVE 40 TO WS-COL-LEN(19).
              MOVE 15 TO WS-COL-LEN(20).
              MOVE 150 TO WS-COL-LEN(21).

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
              MOVE "ADDRESS_LINE"     TO WS-COL-TEXT(21).

           MOVE "|" TO TMP-REC.          *> 標題記錄
           MOVE "|" TO DIVIDING-LINE.    *> 分隔線記錄


           *>   ============  CUSTOMER_ID 標題/分隔線  ============
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(20))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(20) TO WS-FIELD-LEN
                 *> 左邊空白 = (寬度 - 資料長) / 2
                 COMPUTE WS-LEFT-PAD = 
                   FUNCTION INTEGER((WS-FIELD-LEN - WS-DATA-LEN) / 2)
                 COMPUTE WS-RIGHT-PAD =
                   WS-FIELD-LEN - WS-DATA-LEN - WS-LEFT-PAD

                 *> === 填入空白 ===
                 MOVE SPACES TO WS-CENTER-FLD
                 *> 左側填充
                 MOVE SPACES TO WS-CENTER-FLD(1:WS-LEFT-PAD)
                 *> 文字
                 MOVE FUNCTION TRIM(WS-COL-TEXT(20)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC
                 END-STRING


                 MOVE SPACES TO WS-CENTER-FLD
                 INSPECT WS-CENTER-FLD REPLACING ALL " " BY "-"

                 *> === 分隔線 ===
                 STRING
                   FUNCTION TRIM(DIVIDING-LINE)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   "-|" DELIMITED BY SIZE
                   INTO DIVIDING-LINE
                 END-STRING

           *>   ============  ADDRESS_NAME 標題/分隔線  ============
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(21))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(21) TO WS-FIELD-LEN
                 *> 左邊空白 = (寬度 - 資料長) / 2
                 COMPUTE WS-LEFT-PAD = 
                   FUNCTION INTEGER((WS-FIELD-LEN - WS-DATA-LEN) / 2)
                 COMPUTE WS-RIGHT-PAD =
                   WS-FIELD-LEN - WS-DATA-LEN - WS-LEFT-PAD

                 *> === 填入空白 ===
                 MOVE SPACES TO WS-CENTER-FLD
                 *> 左側填充
                 MOVE SPACES TO WS-CENTER-FLD(1:WS-LEFT-PAD)
                 *> 文字
                 MOVE FUNCTION TRIM(WS-COL-TEXT(21)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC
                 END-STRING


                 MOVE SPACES TO WS-CENTER-FLD
                 INSPECT WS-CENTER-FLD REPLACING ALL " " BY "-"

                 *> === 分隔線 ===
                 STRING
                   FUNCTION TRIM(DIVIDING-LINE)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   "-|" DELIMITED BY SIZE
                   INTO DIVIDING-LINE
                 END-STRING


           *> ============  主要欄位 標題/分隔線  ============
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
              IF IDX NOT = 10 AND IDX NOT = 12 *> SUB號碼/樓層略過
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(IDX))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(IDX) TO WS-FIELD-LEN
                 *> 左邊空白 = (寬度 - 資料長) / 2
                 COMPUTE WS-LEFT-PAD = 
                   FUNCTION INTEGER((WS-FIELD-LEN - WS-DATA-LEN) / 2)
                 COMPUTE WS-RIGHT-PAD =
                   WS-FIELD-LEN - WS-DATA-LEN - WS-LEFT-PAD

                 *> === 填入空白 ===
                 MOVE SPACES TO WS-CENTER-FLD
                 *> 左側填充
                 MOVE SPACES TO WS-CENTER-FLD(1:WS-LEFT-PAD)
                 *> 文字
                 MOVE FUNCTION TRIM(WS-COL-TEXT(IDX)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC
                 END-STRING


                 MOVE SPACES TO WS-CENTER-FLD
                 INSPECT WS-CENTER-FLD REPLACING ALL " " BY "-"

                 *> === 分隔線 ===
                 STRING
                   FUNCTION TRIM(DIVIDING-LINE)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   "-|" DELIMITED BY SIZE
                   INTO DIVIDING-LINE
                 END-STRING

              END-IF
           END-PERFORM

       *> 開啟檔案、OUT-FILE
           OPEN OUTPUT OUT-FILE

           *> === 輸出標題 ===
           MOVE TMP-REC TO OUT-FILE-REC
           WRITE OUT-FILE-REC

           *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE TO OUT-FILE-REC
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
                 MOVE SPACES TO OUT-FILE-REC
                 MOVE "|" TO TMP-REC

                   *> CUSTOMER_ID
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     CUSTOMER_ID(IDX)(1:WS-COL-LEN(20))
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                   *> ADDRESS_NAME
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     ADDRESS-FULLNAME(IDX)(1:WS-COL-LEN(21))
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                 *> JDK: 資料欄位
                 PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > 19
                 IF JDX NOT = 10 AND JDX NOT = 12 *> SUB號碼/樓層略過

                   MOVE FUNCTION TRIM(ADDRESS-DTLS(IDX JDX)) TO TMP-DATA
                   MOVE WS-COL-LEN(JDX) TO WS-FIELD-LEN

                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     TMP-DATA(1:WS-FIELD-LEN)
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING
                 END-IF
                 END-PERFORM

                 *> 寫入檔案
                 MOVE TMP-REC TO OUT-FILE-REC
                 WRITE OUT-FILE-REC
           ELSE
             EXIT PERFORM
           END-IF
           END-PERFORM.


           *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE TO OUT-FILE-REC.
           WRITE OUT-FILE-REC.

           *> 總筆數
           MOVE ERROR-COUNT TO ERROR-FMT.
           MOVE DATA-COUNT  TO DATA-FMT.
           STRING
             "TOTAL ITEMS: " DELIMITED BY SIZE
             DATA-FMT DELIMITED BY SIZE
             ", ERROR ITEMS: " DELIMITED BY SIZE
             ERROR-FMT DELIMITED BY SIZE
             INTO TOTAL-COMMENT
           END-STRING.
           COMPUTE TOTAL-LEN = LENGTH OF FUNCTION TRIM(TMP-REC) -
                               LENGTH OF FUNCTION TRIM(TOTAL-COMMENT)
                               - 3.
           MOVE SPACES TO WS-CENTER-FLD.
           STRING 
             "| " DELIMITED BY SIZE
             FUNCTION TRIM(TOTAL-COMMENT) DELIMITED BY SIZE
             WS-CENTER-FLD(1:TOTAL-LEN) DELIMITED BY SIZE
             "|" DELIMITED BY SIZE
             INTO TMP-REC
           END-STRING.

           *> 寫入檔案
           MOVE TMP-REC TO OUT-FILE-REC.
           WRITE OUT-FILE-REC.


       *> 關閉檔案
           CLOSE OUT-FILE


      *******************************************************
      *> 輸出: Fail_Data.txt
      *******************************************************
           *> 標題設定
              MOVE 15  TO WS-COL-LEN(1).
              MOVE 130 TO WS-COL-LEN(2).
              MOVE 40  TO WS-COL-LEN(3).

              MOVE "CUSTOMER_ID"    TO WS-COL-TEXT(1).
              MOVE "ADDRESS_LINE"   TO WS-COL-TEXT(2).
              MOVE "ERROR_MESSAGE"  TO WS-COL-TEXT(3).

           MOVE "|" TO TMP-REC.          *> 標題記錄
           MOVE "|" TO DIVIDING-LINE.    *> 分隔線記錄

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(IDX))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(IDX) TO WS-FIELD-LEN
                 *> 左邊空白 = (寬度 - 資料長) / 2
                 COMPUTE WS-LEFT-PAD = 
                   FUNCTION INTEGER((WS-FIELD-LEN - WS-DATA-LEN) / 2)
                 COMPUTE WS-RIGHT-PAD =
                   WS-FIELD-LEN - WS-DATA-LEN - WS-LEFT-PAD

                 *> === 填入空白 ===
                 MOVE SPACES TO WS-CENTER-FLD
                 *> 左側填充
                 MOVE SPACES TO WS-CENTER-FLD(1:WS-LEFT-PAD)
                 *> 文字
                 MOVE FUNCTION TRIM(WS-COL-TEXT(IDX)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC
                 END-STRING

                 MOVE SPACES TO WS-CENTER-FLD
                 INSPECT WS-CENTER-FLD REPLACING ALL " " BY "-"

                 *> === 分隔線 ===
                 STRING
                   FUNCTION TRIM(DIVIDING-LINE)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   "-|" DELIMITED BY SIZE
                   INTO DIVIDING-LINE
                 END-STRING
           END-PERFORM

       *> 開啟檔案、ERROR-FILE
           OPEN OUTPUT ERROR-FILE

           *> === 輸出標題 ===
           MOVE TMP-REC TO ERROR-REC
           WRITE ERROR-REC

           *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE TO ERROR-REC
           WRITE ERROR-REC

           *> === 輸出內容 ===
           COMPUTE WS-FIELD-LEN 
             = WS-COL-LEN(1) + WS-COL-LEN(2) + WS-COL-LEN(3) + 7

           *> ===============       有錯誤資料       ===============
           IF ERROR-COUNT NOT = 0
           *> IDX: 記錄數迴圈，最多1000行
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 1000
           IF ADDRESS-FULLNAME(IDX)  NOT = SPACES
             IF ERROR-COMMENT(IDX) NOT = SPACES
                 *> 將3個欄位合併為1行寫入
                 MOVE SPACES TO ERROR-REC
                 MOVE "|" TO TMP-REC

                   *> CUSTOMER_ID
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     CUSTOMER_ID(IDX)(1:WS-COL-LEN(1))
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                   *> ADDRESS_NAME
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     ADDRESS-FULLNAME(IDX)(1:WS-COL-LEN(2))
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                   *> ERROR_MESSAGE
                   STRING
                     FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
                     ERROR-COMMENT(IDX)(1:WS-COL-LEN(3))
                       DELIMITED BY SIZE
                     " |" DELIMITED BY SIZE
                     INTO TMP-REC
                   END-STRING

                 *> 寫入檔案
                 MOVE TMP-REC TO ERROR-REC
                 WRITE ERROR-REC
             END-IF
           ELSE
             EXIT PERFORM
           END-IF
           END-PERFORM

             *> === 輸出分隔線 ===
             MOVE DIVIDING-LINE TO ERROR-REC
             WRITE ERROR-REC

             *> 總筆數
             MOVE SPACES TO TOTAL-COMMENT
             STRING
               "ERROR ITEMS: " DELIMITED BY SIZE
               ERROR-FMT DELIMITED BY SIZE
               INTO TOTAL-COMMENT
             END-STRING
             COMPUTE TOTAL-LEN = LENGTH OF FUNCTION TRIM(TMP-REC) - 
                                 LENGTH OF FUNCTION TRIM(TOTAL-COMMENT) 
                                 - 3
             MOVE SPACES TO WS-CENTER-FLD
             STRING 
               "| " DELIMITED BY SIZE
               FUNCTION TRIM(TOTAL-COMMENT) DELIMITED BY SIZE
               WS-CENTER-FLD(1:TOTAL-LEN) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               INTO TMP-REC
             END-STRING

             *> 寫入檔案
             MOVE TMP-REC TO ERROR-REC
             WRITE ERROR-REC

           *> ===============       無錯誤資料       ===============
           ELSE
             MOVE "|" TO TMP-REC
             MOVE "NO ERROR DATA" TO TMP-DATA
             STRING 
               FUNCTION TRIM(TMP-REC) DELIMITED BY SIZE
               TMP-DATA(1:WS-FIELD-LEN - 2) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               INTO TMP-REC
             MOVE TMP-REC TO ERROR-REC
             WRITE ERROR-REC
           END-IF.

       *> 關閉檔案
           CLOSE ERROR-FILE

           EXIT PROGRAM.
       END PROGRAM OUTPUT-ADDRESS.
