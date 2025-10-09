       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXECUTE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.


      *******************************************************
      *> 主要資料群
      *******************************************************
       01 IDX        PIC 99.
       01 JDX        PIC 9999.
       01 ERROR-FLAG PIC X.
       01 ERROR-TEMP PIC X(40).
       01 COMMA-FLAG PIC XX.


      *******************************************************
      *> 函式呼叫用的變數
      *******************************************************
       *> READ-ADDRESS 用（1000行*5列的資料接收）
       01 LS-RA.
           05 INPUT-DATA         PIC X(500) OCCURS 1000 TIMES. *> 讀取資料
           05 FORMATTER-DATA     PIC X(500) OCCURS 1000 TIMES. *> 格式化讀取資料
           05 ADDRESS-LIST-G     OCCURS 1000 TIMES. *> 原資料
              10 ADDRESS-LIST    PIC X(100) OCCURS 5 TIMES. *> 原資料
              10 ERROR-ADDRESS   PIC X(40). *> 錯誤資料
              10 DTLS-LR         PIC X(100) OCCURS 18 TIMES. *> 地址欄位

       *> SPLIT-ADDRESS-FIELDS 用
       01 LS-SAF.
           05 NMADR   PIC X(100).
           05 DTLS    PIC X(100) OCCURS 18 TIMES.
           05 LOOP-NO PIC 9.

       *> LIST-REC 用 (從 LIST.csv 讀取 18行*40列)
       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES. 

       *> OUTPUT-ADDRESS 用 (最多可存1000行紀錄)
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
      *> 程式處理區
      *******************************************************
       PROCEDURE DIVISION.


      *******************************************************
      *> 資料準備
      *******************************************************
           *> LS-RA 呼叫 READ-ADDRESS
           *> LS-LIST-REC 呼叫 READ-RULE
           CALL 'READ-ADDRESS' USING LS-RA LS-LIST-REC.


      *******************************************************
      *> 拆解並設定到各欄位
      *******************************************************
           *> JDX: 紀錄數迴圈，最多1000行
           PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > 1000
                 MOVE FUNCTION TRIM(INPUT-DATA(JDX)) 
                   TO ADDRESS-FULLNAME(JDX)
                 IF INPUT-DATA(JDX) = SPACES
                        EXIT PERFORM
                 END-IF

                 *> DTLS 初期化
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                    MOVE SPACES TO DTLS(IDX)
                 END-PERFORM

                 *> DTLS-LR -> DTLS
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                    MOVE FUNCTION TRIM(DTLS-LR(JDX IDX)) TO DTLS(IDX)
                 END-PERFORM
                 MOVE DTLS-LR(JDX 2) TO DTLS(2)

                 *> 接收欄位清單
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
                    MOVE ADDRESS-LIST(JDX IDX) TO NMADR
                 END-PERFORM


              PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                 MOVE FUNCTION TRIM(DTLS(IDX)) TO ADDRESS-DTLS(JDX IDX)
                 EVALUATE IDX
                    WHEN 1
                          MOVE FUNCTION TRIM(DTLS(1)) TO ZIP(JDX)
                    WHEN 2
                          MOVE FUNCTION TRIM(DTLS(2)) TO COUNTRY(JDX)
                    WHEN 3
                          MOVE FUNCTION TRIM(DTLS(3)) TO CITY(JDX)
                    WHEN 4
                          MOVE FUNCTION TRIM(DTLS(4)) TO DISTRICT(JDX)
                    WHEN 5
                          MOVE FUNCTION TRIM(DTLS(5)) TO STREET(JDX)
                    WHEN 6
                          MOVE FUNCTION TRIM(DTLS(6)) TO SEC(JDX)
                    WHEN 7
                          MOVE FUNCTION TRIM(DTLS(7)) TO LANE(JDX)
                    WHEN 8
                          MOVE FUNCTION TRIM(DTLS(8)) TO ALLEY(JDX)
                    WHEN 9
                          MOVE FUNCTION TRIM(DTLS(9)) TO M-NO(JDX)
                    WHEN 10
                          MOVE FUNCTION TRIM(DTLS(10)) TO S-NO(JDX)
                    WHEN 11
                          MOVE FUNCTION TRIM(DTLS(11)) TO M-FLOOR(JDX)
                    WHEN 12
                          MOVE FUNCTION TRIM(DTLS(12)) TO S-FLOOR(JDX)
                    WHEN 13
                          MOVE FUNCTION TRIM(DTLS(13)) TO ROOM(JDX)
                    WHEN 14
                          MOVE FUNCTION TRIM(DTLS(14)) TO BUILDING(JDX)
                    WHEN 15
                          MOVE FUNCTION TRIM(DTLS(15)) TO VILLAGE(JDX)
                    WHEN 16
                          MOVE FUNCTION TRIM(DTLS(16)) TO PROVINCE(JDX)
                    WHEN 17
                          MOVE FUNCTION TRIM(DTLS(17)) TO STATE(JDX)
                    WHEN 18
                          MOVE FUNCTION TRIM(DTLS(18)) TO OTHER-COL(JDX)
                  END-EVALUATE

              END-PERFORM
              

              *> 客戶統編(暫定)
              MOVE JDX TO CUSTOMER_ID(JDX)
      *******************************************************
      *> 錯誤分類
      *******************************************************
              MOVE "N" TO ERROR-FLAG
              MOVE "PLEASE ENTER" TO ERROR-TEMP
              MOVE SPACES TO COMMA-FLAG

              *> ZIP 為空值
              IF ZIP(JDX) = SPACES
                STRING 
                  FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
                  COMMA-FLAG DELIMITED BY SPACES
                  " POSTAL CODE" DELIMITED BY SIZE
                  INTO ERROR-TEMP
                END-STRING
                MOVE "Y" TO ERROR-FLAG
                MOVE "," TO COMMA-FLAG
              END-IF

              *> COUNTRY 為空值
              IF COUNTRY(JDX) = SPACES
                STRING 
                  FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
                  COMMA-FLAG DELIMITED BY SPACES
                  " COUNTRY" DELIMITED BY SIZE
                  INTO ERROR-TEMP
                END-STRING
                MOVE "Y" TO ERROR-FLAG
                MOVE "," TO COMMA-FLAG
              END-IF

              *> CITY 為空值
              IF CITY(JDX) = SPACES
                STRING 
                  FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
                  COMMA-FLAG DELIMITED BY SPACES
                  " CITY" DELIMITED BY SIZE
                  INTO ERROR-TEMP
                END-STRING
                MOVE "Y" TO ERROR-FLAG
                MOVE "," TO COMMA-FLAG
              END-IF

              *> OTHER 有值: PARSING FAILED. PLEASE CHECK INPUT
              IF OTHER-COL(JDX) NOT = SPACES
                MOVE "PARSING FAILED. PLEASE CHECK INPUT" 
                  TO ERROR-TEMP
                MOVE "Y" TO ERROR-FLAG
              END-IF

              *> 包含特殊字體: CONTAINS INVALID CHARACTERS
              *> 輸入文字過長: ADDRESS DATA IS TOO LONG
              IF ERROR-ADDRESS(JDX) NOT = SPACES
                MOVE ERROR-ADDRESS(JDX) TO ERROR-TEMP
                MOVE "Y" TO ERROR-FLAG
              END-IF

              IF ERROR-FLAG = "Y"
                STRING 
                  FUNCTION TRIM(ERROR-TEMP) DELIMITED BY SIZE
                  "." DELIMITED BY SIZE
                  INTO ERROR-TEMP
                END-STRING
                
                MOVE ERROR-TEMP TO ERROR-COMMENT(JDX)
                MOVE ERROR-TEMP TO ADDRESS-DTLS(JDX 19)
              ELSE
                MOVE SPACES TO ERROR-COMMENT(JDX) ADDRESS-DTLS(JDX 19)
              END-IF

      *******************************************************
      *> 測試輸出
      *******************************************************
      *    DISPLAY "*********************************************"
      *    DISPLAY "RECORD NO. " JDX
      *            " /" FUNCTION TRIM(ADDRESS-FULLNAME(JDX))
      *    DISPLAY "RECORD NO. " JDX
      *            " /" FUNCTION TRIM(FORMATTER-DATA(JDX))
      *
      *     *> 確認地址欄位
      *    DISPLAY "-:+:-:+:-:+:-:+:- :+:-:+: -:+:-:+:-:+:-:+:-"
      *    DISPLAY "-:+:-:+:-:+:-     OUTPUT     -:+:-:+:-:+:-"
      *    DISPLAY "-:+:-:+:-:+:-:+:- :+:-:+: -:+:-:+:-:+:-:+:-"
      *    DISPLAY "01 ZIP     :" ZIP(JDX)
      *    DISPLAY "02 COUNTRY :" COUNTRY(JDX)
      *    DISPLAY "03 CITY    :" CITY(JDX)
      *    DISPLAY "04 DISTRICT:" DISTRICT(JDX)
      *    DISPLAY "05 STREET  :" STREET(JDX)
      *    DISPLAY "06 SEC     :" SEC(JDX)
      *    DISPLAY "07 LANE    :" LANE(JDX)
      *    DISPLAY "08 ALLEY   :" ALLEY(JDX)
      *    DISPLAY "09 M-NO    :" M-NO(JDX)
      **    DISPLAY "10 S-NO    :" S-NO(JDX)
      *    DISPLAY "11 M-FLOOR :" M-FLOOR(JDX)
      **    DISPLAY "12 S-FLOOR :" S-FLOOR(JDX)
      *    DISPLAY "13 ROOM    :" ROOM(JDX)
      *    DISPLAY "14 BUILDING:" BUILDING(JDX)
      *    DISPLAY "15 VILLAGE :" VILLAGE(JDX)
      *    DISPLAY "16 PROVINCE:" PROVINCE(JDX)
      *    DISPLAY "17 STATE   :" STATE(JDX)
      *    DISPLAY "18 OTHER   :" OTHER-COL(JDX)
      *    DISPLAY "19 ERROR   :" ERROR-ADDRESS(JDX)
      *    DISPLAY "19 ERROR   :" ERROR-COMMENT(JDX)

           *> 紀錄拆解處理結束
           END-PERFORM.

      *******************************************************
      *> 輸出資料
      *******************************************************
           *> 呼叫 OUTPUT-ADDRESS
           CALL 'OUTPUT-ADDRESS' USING ADDRESS-DATA.

           *> 呼叫 OUTPUT-ADDRESS-CSV
           CALL 'OUTPUT-ADDRESS-CSV' USING ADDRESS-DATA.

           *> 程式結束
           STOP RUN.
