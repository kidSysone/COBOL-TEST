       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-ADDRESS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT IN-FILE ASSIGN
             TO "input\INPUT-ADDRESS.txt"
      *      TO "input\fake_addresses.txt"
             ORGANIZATION IS LINE SEQUENTIAL
             STATUS IN-FILE-STATUS.


       DATA DIVISION.
      *******************************************************
      *> 資料部、FILE SECTION
      *******************************************************
       FILE SECTION.
       FD IN-FILE.
         01 IN-FILE-REC PIC X(500).


      *******************************************************
      *> 資料部、WORKING-STORAGE SECTION
      *******************************************************
       WORKING-STORAGE SECTION.
         01 IN-FILE-STATUS PIC XX VALUE "00".
         01 IN-FILE-DATA PIC X(500).
         01 IDX PIC 9999 VALUE 1.
         01 JDX PIC 99 VALUE 1.
         01 KDX PIC 9999 VALUE 1.
         01 COMMAS PIC X VALUE ",".

         01 WS-TRIMMED           PIC X(100).
         01 ADDRESS-SPLIT        PIC X(100) OCCURS 18 TIMES.
         01 ADDRESS-LIST-LEN     PIC 999.

       *> 特殊字判斷
       01 WS-CH                  PIC X.
       01 WS-CODE                PIC 9(5).
       01 ALLOWED-CH             PIC X(20) VALUE "/-?:().,+'".
       01 ERROR-FLAG             PIC X.

      *******************************************************
      *> 函式呼叫用的變數
      *******************************************************
       *> FORMATTER-ADDRESS 用
       01 LS-FORMATTER.
           05 BEFORE-DATA PIC X(500). *> 格式化讀取資料
           05 AFTER-DATA  PIC X(500). *> 格式化回傳資料
           05 DTLS-LF     PIC X(100) OCCURS 18 TIMES. *> 地址欄位

      *******************************************************
      *> 資料部、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       *> 接收 1000 行 * 5 欄 的資料
       01 LS-RA.
           05 INPUT-DATA         PIC X(500) OCCURS 1000 TIMES. *> 讀取資料
           05 FORMATTER-DATA     PIC X(500) OCCURS 1000 TIMES. *> 格式化讀取資料
           05 ADDRESS-LIST-G     OCCURS 1000 TIMES. *> 原資料
              10 ADDRESS-LIST    PIC X(100) OCCURS 5 TIMES. *> 原資料
              10 ERROR-ADDRESS   PIC X(40). *> 錯誤資料
              10 DTLS-LR         PIC X(100) OCCURS 18 TIMES. *> 地址欄位

       *> LIST-REC 用 (從 LIST.csv 讀取 18行*40列)
       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES. 

      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-RA LS-LIST-REC.
       MAIN SECTION.
           *> 呼叫 READ-RULE
           CALL 'READ-RULE' USING LS-LIST-REC.


           *> ==== 檔案匯入，每次讀取一行到 IN-FILE-REC ====
           MOVE 1 TO IDX.
           OPEN INPUT IN-FILE.
           PERFORM UNTIL IN-FILE-STATUS = "10"
             READ IN-FILE
               AT END *> 當已無資料可讀取（EOF）
                 MOVE "10" TO IN-FILE-STATUS
               NOT AT END *> 當讀取成功時
                 MOVE IN-FILE-REC TO INPUT-DATA(IDX)
                 ADD 1 TO IDX
           END-READ
           END-PERFORM.
           CLOSE IN-FILE.

            *> ==== 將讀入的資料設定到 ADDRESS-LIST ====
           PERFORM VARYING KDX FROM 1 BY 1 UNTIL KDX > 1000
                 *> 若讀入資料為空白則結束
                 IF INPUT-DATA(KDX) = SPACES
                   EXIT PERFORM 
                 END-IF

                 *> 將 INPUT-DATA 的內容 FORMATTER
                 MOVE INPUT-DATA(KDX) TO BEFORE-DATA
                 CALL 'FORMATTER-ADDRESS' USING LS-LIST-REC LS-FORMATTER
                 MOVE AFTER-DATA TO FORMATTER-DATA(KDX)
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                    MOVE DTLS-LF(IDX) TO DTLS-LR(KDX IDX)
                 END-PERFORM

                 *> ADDRESS-SPLIT 初期化
                 PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
                    MOVE SPACES TO ADDRESS-SPLIT(IDX)
                 END-PERFORM

                 *> 將 INPUT-DATA 的內容分割
                 UNSTRING FORMATTER-DATA(KDX)
                     DELIMITED BY ","
                     INTO ADDRESS-SPLIT (1)
                          ADDRESS-SPLIT (2)
                          ADDRESS-SPLIT (3)
                          ADDRESS-SPLIT (4)
                          ADDRESS-SPLIT (5)
                          ADDRESS-SPLIT (6)
                          ADDRESS-SPLIT (7)
                          ADDRESS-SPLIT (8)
                          ADDRESS-SPLIT (9)
                          ADDRESS-SPLIT (10)
                          ADDRESS-SPLIT (11)
                          ADDRESS-SPLIT (12)
                          ADDRESS-SPLIT (13)
                          ADDRESS-SPLIT (14)
                          ADDRESS-SPLIT (15)
                          ADDRESS-SPLIT (16)
                          ADDRESS-SPLIT (17)
                          ADDRESS-SPLIT (18)


           *> ADDRESS-SPLIT -> ADDRESS-LIST
           MOVE SPACES TO ADDRESS-LIST(KDX 1) ADDRESS-LIST(KDX 2)
                          ADDRESS-LIST(KDX 3) ADDRESS-LIST(KDX 4)
                          ADDRESS-LIST(KDX 5)

           MOVE FUNCTION TRIM(ADDRESS-SPLIT(1)) TO WS-TRIMMED
           STRING
               WS-TRIMMED DELIMITED BY SIZE
               INTO ADDRESS-LIST(KDX 1)
           END-STRING
           MOVE LENGTH OF FUNCTION TRIM(WS-TRIMMED) TO ADDRESS-LIST-LEN

           MOVE 1 TO JDX
             *> 若 TRIM(ADDRESS-SPLIT(IDX)) 字數 > 35 -> ERROR
           IF LENGTH OF FUNCTION TRIM(ADDRESS-SPLIT(1)) > 35
             MOVE "ADDRESS DATA IS TOO LONG" TO ERROR-ADDRESS(KDX)
           END-IF

           PERFORM VARYING IDX FROM 2 BY 1 UNTIL IDX > 18
             MOVE FUNCTION TRIM(ADDRESS-SPLIT(IDX)) TO WS-TRIMMED
             COMPUTE ADDRESS-LIST-LEN = ADDRESS-LIST-LEN +
               LENGTH OF FUNCTION TRIM(WS-TRIMMED) + 1
            
             *> 若 TRIM(ADDRESS-SPLIT(IDX)) 字數 > 35 -> ERROR
             IF LENGTH OF FUNCTION TRIM(ADDRESS-SPLIT(IDX)) > 35
               MOVE "ADDRESS DATA IS TOO LONG" TO ERROR-ADDRESS(KDX)
             END-IF

             *> 若 ADDRESS-LIST-LEN <= 35，則追加到同一個欄位
             IF ADDRESS-LIST-LEN <= 35
                 *> ADDRESS-LIST(KDX  JDX) = 
                 *> ADDRESS-LIST(KDX  JDX) + "," + WS-TRIMMED
                 STRING
                     FUNCTION TRIM(ADDRESS-LIST(KDX JDX))
                       DELIMITED BY SIZE
                     COMMAS DELIMITED BY SIZE
                     WS-TRIMMED DELIMITED BY SIZE
                     INTO ADDRESS-LIST(KDX JDX)
                 END-STRING

             *> 若 ADDRESS-LIST-LEN > 35，則放到下一個欄位
             ELSE
                 ADD 1 TO JDX
                 MOVE WS-TRIMMED TO ADDRESS-LIST(KDX JDX)
                 COMPUTE ADDRESS-LIST-LEN = 
                   LENGTH OF FUNCTION TRIM(WS-TRIMMED)
             END-IF
           END-PERFORM
              DISPLAY "ERROR? "LENGTH OF FUNCTION TRIM(INPUT-DATA(KDX))

             *> 若包含特殊字體
             MOVE "N" TO ERROR-FLAG
             PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 
               LENGTH OF FUNCTION TRIM(INPUT-DATA(KDX))

               MOVE INPUT-DATA(KDX)(IDX:1) TO WS-CH
               COMPUTE WS-CODE = FUNCTION ORD(WS-CH)
               IF WS-CODE < 32 OR WS-CODE > 126
                 MOVE "Y" TO ERROR-FLAG
                 EXIT PERFORM
               ELSE
                 INSPECT ALLOWED-CH TALLYING WS-CODE FOR ALL WS-CH
                 IF NOT((WS-CH >= "0" AND WS-CH <= "9") OR 
                    (WS-CH >= "A" AND WS-CH <= "Z") OR
                    (WS-CH >= "a" AND WS-CH <= "z") OR
                    WS-CH = SPACE OR
                    WS-CODE > 0)
                     MOVE "Y" TO ERROR-FLAG
                     EXIT PERFORM
                 END-IF
               END-IF
             END-PERFORM
             IF ERROR-FLAG = "Y"
              MOVE "CONTAINS INVALID CHARACTERS" TO ERROR-ADDRESS(KDX)
               EXIT PERFORM
             END-IF

      *    DISPLAY "-:+:-:+:-:+:-:+:-:+:-:+:-:+:-:+:-:+:-:+:-"
      *    DISPLAY "INPUT:        " FUNCTION TRIM(INPUT-DATA(KDX))
      *    DISPLAY "INPUT:        " FUNCTION TRIM(FORMATTER-DATA(KDX))
      *    DISPLAY "ADDRESS-LIST: " ADDRESS-LIST(KDX 1)
      *    DISPLAY "ADDRESS-LIST: " ADDRESS-LIST(KDX 2)
      *    DISPLAY "ADDRESS-LIST: " ADDRESS-LIST(KDX 3)
      *    DISPLAY "ADDRESS-LIST: " ADDRESS-LIST(KDX 4)
      *    DISPLAY "ADDRESS-LIST: " ADDRESS-LIST(KDX 5)
      *    DISPLAY "-:+:-:+:-:+:-:+:- :+:-:+: -:+:-:+:-:+:-:+:-"

           END-PERFORM.

           *> 處理結束
           EXIT PROGRAM.
       END PROGRAM READ-ADDRESS.
