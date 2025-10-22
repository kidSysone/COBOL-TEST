       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXECUTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      *******************************************************
      *> 環境部
      *> IN-FILE
      *> 檔案名稱：INPUT-ADDRESS.csv
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）
      *******************************************************
           SELECT IN-FILE ASSIGN
             TO "input\INPUT-ADDRESS.csv"
             ORGANIZATION IS LINE SEQUENTIAL
             STATUS IN-FILE-STATUS.

      *******************************************************
      *> OUT-FILE-CSV
      *> 檔案名稱：Address_Split.csv
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）

      *> ERROR-FILE-CSV
      *> 檔案名稱：Fail_Data.csv
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）
      *******************************************************
           SELECT OUT-FILE-CSV ASSIGN 
             TO 'output\Address_Split.csv'
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE-CSV ASSIGN
             TO 'output\Fail_Data.csv'
             ORGANIZATION IS LINE SEQUENTIAL.

      *******************************************************
      *> 檔案名稱：Address_Split.txt
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）

      *> 檔案名稱：Fail_Data.txt
      *> 檔案組織：LINE SEQUENTIAL（以行為單位）
      *******************************************************
           SELECT OUT-FILE ASSIGN 
             TO 'output\Address_Split.txt'
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERROR-FILE ASSIGN
             TO 'output\Fail_Data.txt'
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *******************************************************
      *> 資料部、FILE SECTION
      *******************************************************
       FILE SECTION.
       FD IN-FILE.
         01 IN-FILE-REC PIC X(500).

       FD OUT-FILE-CSV.
         01 OUT-FILE-REC-CSV PIC X(2000).

       FD ERROR-FILE-CSV.
         01 ERROR-REC-CSV PIC X(2000).

       FD OUT-FILE.
         01 OUT-FILE-REC PIC X(2000).

       FD ERROR-FILE.
         01 ERROR-REC PIC X(2000).

      *******************************************************
      *> 資料部、WORKING-STORAGE SECTION
      *******************************************************
       WORKING-STORAGE SECTION.
       *> 單筆資料
       01 IFS.
         05 IN-FILE-STATUS PIC XX VALUE "00".
         05 IF-DATA PIC X(2000) OCCURS 1000 TIMES.
            *> 1: CUSTOMER_ID
            *> 2: ADDR_LINE_ORIG
            *> 3: ADDR_LINE_EN

       01 IDX PIC 9999 VALUE 1.

       *> ========= OUT-FILE-CSV =========
       *> === TOTAL ===
       01 TOTAL-DATA.
              05 TMP-REC-TOTAL PIC X(2000).
              05 DATA-COUNT    PIC 9999 VALUE 0.
              05 DATA-FMT      PIC ZZZ9.
              05 TOTAL-COMMENT PIC X(2000).

       *> === ERROR ===
       01 ERROR-DATA.
              05 TMP-REC-ERROR PIC X(2000).
              05 ERROR-COUNT   PIC 9999 VALUE 0.
              05 ERROR-FMT     PIC ZZZ9.

       *> ========= OUT-FILE-TXT =========
       01 TITLE-ADDRESS-DATA-TXT.
              05 WS-FIELD-LEN  PIC 999  VALUE 35.   *> 欄位寬度
              05 WS-DATA-LEN   PIC 999.             *> 資料長度
              05 WS-LEFT-PAD   PIC 999.             *> 左邊空白數
              05 WS-RIGHT-PAD  PIC 999.             *> 右邊空白數
              05 WS-CENTER-FLD PIC X(2000).         *> 輸出欄位
              05 TMP-REC-TXT   PIC X(2000).
              05 DIVIDING-LINE PIC X(2000).
              05 TOTAL-LEN     PIC 9999 VALUE 0.
       01 ERROR-DATA-TXT.
              05 TMP-REC-TXT-ERROR   PIC X(2000).
              05 DIVIDING-LINE-ERROR PIC X(2000).
              05 TMP-DATA            PIC X(500).

      *******************************************************
      *> 呼叫函式用的變數
      *******************************************************
       *> FORMATTER-ADDRESS 用
       01 LS-FORMATTER.
           05 BEFORE-DATA  PIC X(500). *> 格式化讀取資料
           05 AFTER-DATA   PIC X(500). *> 格式化回傳資料
           05 ORIGIN-DATA  PIC X(500). *> 原文
           05 DTLS-LF      PIC X(500) OCCURS 25 TIMES. *> 地址欄位

       *> READ-RULE 用 (從 LIST.csv 讀取 18行*40列)
       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-STATE-NAME-COL    PIC X(45) OCCURS 200 TIMES.
           05  LS-STATE-CODE-COL    PIC X(10) OCCURS 200 TIMES.
           05  DIR-NAMES OCCURS 21 TIMES PIC X(8). *> 全方向

       *> OUTPUT-ADDRESS 用
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
       PROCEDURE DIVISION.
       MAIN SECTION.

           *> 輸出格式設定(欄位寬度/欄位標題)
              MOVE 35  TO WS-COL-LEN(1).
              MOVE 35  TO WS-COL-LEN(2).
              MOVE 100 TO WS-COL-LEN(3).
              MOVE 100 TO WS-COL-LEN(4).
              MOVE 50  TO WS-COL-LEN(5).
              MOVE 35  TO WS-COL-LEN(6).
              MOVE 50  TO WS-COL-LEN(7).
              MOVE 35  TO WS-COL-LEN(8).
              MOVE 35  TO WS-COL-LEN(9).
              MOVE 35  TO WS-COL-LEN(10).
              MOVE 35  TO WS-COL-LEN(11).
              MOVE 35  TO WS-COL-LEN(12).
              MOVE 35  TO WS-COL-LEN(13).
              MOVE 35  TO WS-COL-LEN(14).
              MOVE 35  TO WS-COL-LEN(15).
              MOVE 35  TO WS-COL-LEN(16).
              MOVE 10  TO WS-COL-LEN(17).
              MOVE 35  TO WS-COL-LEN(18).
              MOVE 40  TO WS-COL-LEN(19).
              MOVE 15  TO WS-COL-LEN(20).
              MOVE 150 TO WS-COL-LEN(21).
              MOVE 100 TO WS-COL-LEN(22).
              MOVE 100 TO WS-COL-LEN(23).

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
              MOVE "ERROR_MESSAGE"            TO WS-COL-TEXT(19).
              MOVE "CUSTOMER_ID"              TO WS-COL-TEXT(20).
              MOVE "ADDRESS_LINE_ORIGIN"      TO WS-COL-TEXT(21).
              MOVE "ADDRESS_LINE_EN"          TO WS-COL-TEXT(22).
              MOVE "ADDRESS_LINE_REBUILD"     TO WS-COL-TEXT(23).

              MOVE WS-COL-LEN(20)  TO WS-COL-LEN-ERROR(1). *> 20 客戶 ID
              MOVE WS-COL-LEN(21)  TO WS-COL-LEN-ERROR(2). *> 21 讀取_原文
              MOVE WS-COL-LEN(22)  TO WS-COL-LEN-ERROR(3). *> 22 讀取_英文 
              MOVE WS-COL-LEN(19)  TO WS-COL-LEN-ERROR(4). *> 19 錯誤

              MOVE "CUSTOMER_ID"           TO WS-COL-TEXT-ERROR(1).
              MOVE "ADDRESS_LINE_ORIGIN"   TO WS-COL-TEXT-ERROR(2).
              MOVE "ADDRESS_LINE_EN"       TO WS-COL-TEXT-ERROR(3).
              MOVE "ERROR_MESSAGE"         TO WS-COL-TEXT-ERROR(4).


      *******************************************************
      *> 標題設置: Address_Split.csv
      *******************************************************
           STRING
             FUNCTION TRIM(WS-COL-TEXT(20)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(WS-COL-TEXT(21)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             FUNCTION TRIM(WS-COL-TEXT(22)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC-TOTAL
           END-STRING.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 19
             IF IDX NOT = 10 AND IDX NOT = 12 *> SUB號碼/樓層略過
               STRING
                 FUNCTION TRIM(TMP-REC-TOTAL) DELIMITED BY SIZE
                 FUNCTION TRIM(WS-COL-TEXT(IDX)) DELIMITED BY SIZE
                 ";" DELIMITED BY SIZE
                 INTO TMP-REC-TOTAL
               END-STRING
             END-IF
           END-PERFORM.

           STRING
             FUNCTION TRIM(TMP-REC-TOTAL) DELIMITED BY SIZE
             FUNCTION TRIM(WS-COL-TEXT(23)) DELIMITED BY SIZE
             ";" DELIMITED BY SIZE
             INTO TMP-REC-TOTAL
           END-STRING.

      *******************************************************
      *> 標題設置: Fail_Data.csv
      *******************************************************
              MOVE SPACES TO ERROR-REC-CSV TMP-REC-ERROR.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
                 *> === TMP-REC ===
                 STRING
                   FUNCTION TRIM(TMP-REC-ERROR) DELIMITED BY SIZE
                   FUNCTION TRIM(WS-COL-TEXT-ERROR(IDX))
                     DELIMITED BY SIZE
                   ";" DELIMITED BY SIZE
                   INTO TMP-REC-ERROR
                 END-STRING
           END-PERFORM


      *******************************************************
      *> 標題設置: Address_Split.txt
      *******************************************************
           MOVE "|" TO TMP-REC-TXT.      *> 標題記錄
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

                 *> === TMP-REC-TXT ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT
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

           *>   ============  ADDR_LINE_ORIG 標題/分隔線  ============
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

                 *> === TMP-REC-TXT ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT
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

           *>   ============  ADDR_LINE_EN 標題/分隔線  ============
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(22))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(22) TO WS-FIELD-LEN
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
                 MOVE FUNCTION TRIM(WS-COL-TEXT(22)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC-TXT ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT
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

                 *> === TMP-REC-TXT ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT
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

           *>   ============  ADDR_LINE_REBUILD 標題/分隔線  ============
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT(23))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN(23) TO WS-FIELD-LEN
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
                 MOVE FUNCTION TRIM(WS-COL-TEXT(23)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC-TXT ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT
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

      *******************************************************
      *> 標題設置: Fail_Data.txt
      *******************************************************
           MOVE "|" TO TMP-REC-TXT-ERROR.          *> 標題記錄
           MOVE "|" TO DIVIDING-LINE-ERROR.        *> 分隔線記錄

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
                 *> === 計算長度 ===
                 MOVE LENGTH OF FUNCTION TRIM(WS-COL-TEXT-ERROR(IDX))
                   TO WS-DATA-LEN *> 取得資料長度
                 MOVE WS-COL-LEN-ERROR(IDX) TO WS-FIELD-LEN
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
                 MOVE FUNCTION TRIM(WS-COL-TEXT-ERROR(IDX)) TO
                   WS-CENTER-FLD(WS-LEFT-PAD + 1 : WS-DATA-LEN)
                 *> 右側填充
                 MOVE SPACES TO WS-CENTER-FLD(WS-LEFT-PAD +
                                        WS-DATA-LEN + 1 : WS-RIGHT-PAD)

                 *> === TMP-REC-TXT-ERROR ===
                 STRING
                   FUNCTION TRIM(TMP-REC-TXT-ERROR)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   " |" DELIMITED BY SIZE
                   INTO TMP-REC-TXT-ERROR
                 END-STRING

                 MOVE SPACES TO WS-CENTER-FLD
                 INSPECT WS-CENTER-FLD REPLACING ALL " " BY "-"

                 *> === 分隔線 ===
                 STRING
                   FUNCTION TRIM(DIVIDING-LINE-ERROR)
                   WS-CENTER-FLD(1:WS-FIELD-LEN) DELIMITED BY SIZE
                   "-|" DELIMITED BY SIZE
                   INTO DIVIDING-LINE-ERROR
                 END-STRING
           END-PERFORM


      *******************************************************
      *> 輸出: 標題完成，準備開啟檔案
      *******************************************************
       *> 開啟檔案、OUT-FILE-CSV
           OPEN OUTPUT OUT-FILE-CSV.

           *> === 輸出標題 ===
           MOVE TMP-REC-TOTAL(1:
                            LENGTH OF FUNCTION TRIM(TMP-REC-TOTAL) - 1) 
             TO OUT-FILE-REC-CSV.
           WRITE OUT-FILE-REC-CSV.


       *> 開啟檔案、ERROR-FILE-CSV
           OPEN OUTPUT ERROR-FILE-CSV

           *> === 輸出標題 ===
           MOVE TMP-REC-ERROR(1:
                             LENGTH OF FUNCTION TRIM(TMP-REC-ERROR) - 1) 
             TO ERROR-REC-CSV
           WRITE ERROR-REC-CSV

        *> 開啟檔案、OUT-FILE
           OPEN OUTPUT OUT-FILE
       
            *> === 輸出標題 ===
           MOVE TMP-REC-TXT TO OUT-FILE-REC
           WRITE OUT-FILE-REC
       
            *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE TO OUT-FILE-REC
           WRITE OUT-FILE-REC
       
        *> 開啟檔案、ERROR-FILE
           OPEN OUTPUT ERROR-FILE
       
            *> === 輸出標題 ===
           MOVE TMP-REC-TXT-ERROR TO ERROR-REC
           WRITE ERROR-REC
       
            *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE-ERROR TO ERROR-REC
           WRITE ERROR-REC


      *******************************************************
      *>  開始讀取 IN-FILE.csv
      *******************************************************
           *> 呼叫 READ-RULE
           CALL 'READ-RULE' USING LS-LIST-REC.


           *> ==== 檔案匯入，每次讀取一行到 IN-FILE-REC ====
           OPEN INPUT IN-FILE.
           READ IN-FILE.
           PERFORM UNTIL IN-FILE-STATUS = "10"
             READ IN-FILE
               AT END *> 當已無資料可讀取（EOF）
                 MOVE "10" TO IN-FILE-STATUS
               NOT AT END *> 當讀取成功時
               MOVE SPACES TO IF-DATA(1) IF-DATA(2) IF-DATA(3)

              *> 儲存各項目
                *> 1: CUSTOMER_ID
                *> 2: ADDR_LINE_ORIG
                *> 3: ADDR_LINE_EN
                  UNSTRING IN-FILE-REC
                      DELIMITED BY ";"
                      INTO IF-DATA(1)
                           IF-DATA(2)
                           IF-DATA(3)

              IF IF-DATA(3) = SPACES
                EXIT PERFORM
              END-IF

              ADD 1 TO DATA-COUNT
              DISPLAY "============== NO." DATA-COUNT  " =============="
              DISPLAY "DATA-ORIG: "FUNCTION TRIM(IF-DATA(2))
              DISPLAY "DATA-EN  : "FUNCTION TRIM(IF-DATA(3))

              MOVE IF-DATA(1) TO DTLS-LF(20)
              MOVE IF-DATA(2) TO DTLS-LF(21)
              MOVE IF-DATA(3) TO DTLS-LF(22)


      *******************************************************
      *> 分割處理
      *******************************************************
               MOVE DTLS-LF(22) TO BEFORE-DATA
               MOVE DTLS-LF(22) TO ORIGIN-DATA

             *> 將 INPUT-DATA 的內容 FORMATTER
               CALL 'FORMATTER-ADDRESS' USING LS-LIST-REC LS-FORMATTER


      *******************************************************
      *> 輸出資料
      *******************************************************
             *> 呼叫 OUTPUT-ADDRESS
             MOVE SPACES TO OUT-FILE-REC-CSV TMP-TOTAL
             CALL 'OUTPUT-ADDRESS'     USING LS-FORMATTER LS-OUTPUT

             *> 寫入 OUT-FILE-REC-CSV OUT-FILE-REC
             MOVE TMP-TOTAL TO OUT-FILE-REC-CSV
             WRITE OUT-FILE-REC-CSV

             MOVE TMP-TOTAL-TXT TO OUT-FILE-REC
             WRITE OUT-FILE-REC

             *> 有錯誤訊息，寫入 ERROR-FILE-CSV ERROR-REC
             IF DTLS-LF(19) NOT = SPACES
               ADD 1 TO ERROR-COUNT

               *> 寫入檔案
               MOVE TMP-ERROR TO ERROR-REC-CSV
               WRITE ERROR-REC-CSV

               MOVE TMP-ERROR-TXT TO ERROR-REC
               WRITE ERROR-REC
             END-IF


      *******************************************************
      *> 第 * 筆資料處理完成，準備進入下一筆
      *******************************************************
           END-READ
           END-PERFORM.
           CLOSE IN-FILE.


      *******************************************************
      *> 輸出: Address_Split.csv
           *> 總筆數
      *******************************************************
           MOVE SPACES TO TOTAL-COMMENT OUT-FILE-REC-CSV.
           MOVE ERROR-COUNT TO ERROR-FMT.
           MOVE DATA-COUNT  TO DATA-FMT.
           STRING
             ";RESULT SUMMARY->;TOTAL ITEMS: " DELIMITED BY SIZE
             DATA-FMT DELIMITED BY SIZE
             ", ERROR ITEMS: " DELIMITED BY SIZE
             ERROR-FMT DELIMITED BY SIZE
             INTO TOTAL-COMMENT
           END-STRING.

           *> 寫入檔案
           MOVE TOTAL-COMMENT TO OUT-FILE-REC-CSV.
           WRITE OUT-FILE-REC-CSV.


       *> 關閉檔案
           CLOSE OUT-FILE-CSV.


      *******************************************************
      *> 輸出: Fail_Data.csv
           *> 總筆數
      *******************************************************
           IF ERROR-COUNT > 0
             MOVE SPACES TO TMP-REC-ERROR
             STRING
               ";RESULT SUMMARY->;ERROR ITEMS: " DELIMITED BY SIZE
               ERROR-FMT DELIMITED BY SIZE
               INTO TMP-REC-ERROR
             END-STRING

             *> 寫入檔案
             MOVE TMP-REC-ERROR TO ERROR-REC-CSV
             WRITE ERROR-REC-CSV

           *> ===============       無錯誤資料       ===============
           ELSE
             MOVE ";RESULT SUMMARY->;NO ERROR DATA" TO TMP-REC-ERROR
             MOVE TMP-REC-ERROR TO ERROR-REC-CSV
             WRITE ERROR-REC-CSV
           END-IF.

       *> 關閉檔案
           CLOSE ERROR-FILE-CSV.


      *******************************************************
      *> 輸出: Address_Split.txt
           *> 總筆數
      *******************************************************
           *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE TO OUT-FILE-REC.
           WRITE OUT-FILE-REC.

           *> 總筆數
           MOVE SPACES TO TOTAL-COMMENT.
           STRING
             "TOTAL ITEMS: " DELIMITED BY SIZE
             DATA-FMT DELIMITED BY SIZE
             ", ERROR ITEMS: " DELIMITED BY SIZE
             ERROR-FMT DELIMITED BY SIZE
             INTO TOTAL-COMMENT
           END-STRING.

           COMPUTE TOTAL-LEN = LENGTH OF FUNCTION TRIM(TMP-TOTAL-TXT) -
                               LENGTH OF FUNCTION TRIM(TOTAL-COMMENT)
                               - 3.
           MOVE SPACES TO OUT-FILE-REC TMP-TOTAL-TXT WS-CENTER-FLD.
           STRING 
             "| " DELIMITED BY SIZE
             FUNCTION TRIM(TOTAL-COMMENT) DELIMITED BY SIZE
             WS-CENTER-FLD(1:TOTAL-LEN) DELIMITED BY SIZE
             "|" DELIMITED BY SIZE
             INTO TMP-TOTAL-TXT
           END-STRING.

           *> 寫入檔案
           MOVE TMP-TOTAL-TXT TO OUT-FILE-REC.
           WRITE OUT-FILE-REC.


       *> 關閉檔案
           CLOSE OUT-FILE.


      *******************************************************
      *> 輸出: Fail_Data.csv
           *> 總筆數
      *******************************************************
           *> === 輸出內容 ===
           IF ERROR-COUNT > 0
            *> === 輸出分隔線 ===
           MOVE DIVIDING-LINE-ERROR TO ERROR-REC
           WRITE ERROR-REC
          
             *> 總筆數
             MOVE SPACES TO TOTAL-COMMENT
             STRING
               "ERROR ITEMS: " DELIMITED BY SIZE
               ERROR-FMT DELIMITED BY SIZE
               INTO TOTAL-COMMENT
             END-STRING
             COMPUTE TOTAL-LEN = 
                 LENGTH OF FUNCTION TRIM(DIVIDING-LINE-ERROR)
               - LENGTH OF FUNCTION TRIM(TOTAL-COMMENT)
               - 3
             MOVE SPACES TO WS-CENTER-FLD TMP-ERROR-TXT
             STRING 
               "| " DELIMITED BY SIZE
               FUNCTION TRIM(TOTAL-COMMENT) DELIMITED BY SIZE
               WS-CENTER-FLD(1:TOTAL-LEN) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               INTO TMP-ERROR-TXT
             END-STRING

             *> 寫入檔案
             MOVE TMP-ERROR-TXT TO ERROR-REC
             WRITE ERROR-REC

           *> ===============       無錯誤資料       ===============
           ELSE
             MOVE "|" TO TMP-ERROR-TXT
             MOVE "NO ERROR DATA" TO TMP-DATA
             STRING 
               FUNCTION TRIM(TMP-ERROR-TXT) DELIMITED BY SIZE
               TMP-DATA(1:WS-FIELD-LEN - 2) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               INTO TMP-ERROR-TXT

             *> 寫入檔案
             MOVE TMP-ERROR-TXT TO ERROR-REC
             WRITE ERROR-REC
           END-IF.


       *> 關閉檔案
           CLOSE ERROR-FILE.

           *> 程式結束
           STOP RUN.
