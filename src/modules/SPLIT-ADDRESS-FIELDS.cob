       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPLIT-ADDRESS-FIELDS.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
      *******************************************************
      *> 資料區、LOCAL-STORAGE SECTION
      *******************************************************
       LOCAL-STORAGE SECTION.
       01 TEMP-ARRAY.
           05 TEMP-PART       PIC X(100) OCCURS 18 TIMES.
           05 TEMP-A          PIC X(100).
           05 TEMP-A-LEN      PIC 9.
           05 TEMP-A-CNT      PIC 9.
           05 TEMP-A-CNT-U    PIC X.
           05 TEMP-B          PIC X(100).
           05 TEMP-B-LEN      PIC 9.
           05 TEMP-B-CNT      PIC 9.
           05 TEMP-B-CNT-U    PIC X.
           05 TP-NAME PIC X(100).
           05 TP-LEN PIC 99.
           05 TC-LEN PIC 99.
       01 IDX PIC 99.
       01 JDX PIC 999.
       01 KDX PIC 99.
       01 LDX PIC 99.

       01 CNT      PIC 9.
       01 CNT-U    PIC X.
       01 TEMP-COL PIC X(100).

       01 TEMP-FLAG        PIC X VALUE "N".
       01 MATCH-FLAG       PIC X OCCURS 18 TIMES. *> TEMP-PART格納FLAG

       *> 從 CITY 擷取郵遞區號（英國/加拿大格式）
       01 CITY-PARTS.
           05 LEFT-CNT         PIC 99.
           05 RIGHT-CNT        PIC 99.


       01 PARA-NUMBER-PARTS.
           05 PR-STR       PIC X(35).
           05 PR-FLAG      PIC 999.

      *******************************************************
      *> 資料區、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-SAF.
           05 NMADR   PIC X(100).
           05 DTLS    PIC X(100) OCCURS 18 TIMES.
           05 LOOP-NO PIC 9.

       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES. 


      *******************************************************
      *> 程序部
      *******************************************************
       PROCEDURE DIVISION USING LS-SAF LS-LIST-REC.
           *> TEMP-PART 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
              MOVE SPACES TO TEMP-PART(IDX) 
           END-PERFORM.

           *> 資料分類
           IF NMADR NOT = ALL LOW-VALUES
                 UNSTRING NMADR
                     DELIMITED BY ","
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
           END-IF.


      *******************************************************
      *> 欄位分類、不省略名稱
      *******************************************************
           *> MATCH-FLAG 初期化
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
              MOVE SPACES TO MATCH-FLAG(IDX)
           END-PERFORM.

           *> ==== 逐筆記錄處理 ====
           *> IDK: 檢查 LIST-REC 每一行
           *> JDK: 檢查 地址資料 每一欄
           *> KDK: 檢查 LIST-REC 每一欄
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
            IF (DTLS(IDX) = SPACES AND IDX NOT = 18) OR IDX = 18

            PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > 18
            MOVE FUNCTION UPPER-CASE(TEMP-PART(JDX)) TO TP-NAME

            PERFORM VARYING KDX FROM 2 BY 1 UNTIL KDX > 40
            MOVE FUNCTION TRIM(LS-LIST-COL(IDX KDX)) TO TEMP-COL
            
            IF TP-NAME NOT = SPACES AND TEMP-COL NOT = SPACE
             *> 以下判斷 TP-NAME 是否包含 TEMP-COL
             *> 若 TRUE，執行以下處理
             *> MOVE FUNCTION TRIM(TEMP-PART(JDX)) TO DTLS(IDX)

           *> ==== 計算實際長度 ====
           MOVE 0 TO TP-LEN
           INSPECT FUNCTION REVERSE(TP-NAME)
               TALLYING TP-LEN FOR LEADING SPACES
           SUBTRACT TP-LEN FROM LENGTH OF TP-NAME GIVING TP-LEN

           MOVE 0 TO TC-LEN
           INSPECT FUNCTION REVERSE(TEMP-COL)
               TALLYING TC-LEN FOR LEADING SPACES
           SUBTRACT TC-LEN FROM LENGTH OF TEMP-COL GIVING TC-LEN

           *> ==== 部分匹配檢查 ====
           MOVE 0 TO CNT
           PERFORM VARYING LDX FROM 1 BY 1 UNTIL LDX >
             (TP-LEN - TC-LEN + 1)
               IF TP-NAME(LDX : TC-LEN) = TEMP-COL
                   ADD 1 TO CNT
               END-IF
           END-PERFORM

           IF CNT > 0 AND FUNCTION TRIM(TEMP-PART(JDX)) NOT = SPACE
               MOVE FUNCTION TRIM(TEMP-PART(JDX)) TO DTLS(IDX)
               MOVE "Y" TO MATCH-FLAG(JDX)
               EXIT PERFORM
           END-IF

            *> 判斷結束
            END-IF
            END-PERFORM
            END-PERFORM
            END-IF
           END-PERFORM.
           *> ==== 逐筆記錄處理結束 ====


           *> =============== OTHER CHECK ===============
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 18
              MOVE 0 TO PR-FLAG
              IF MATCH-FLAG(IDX) NOT = "Y"
                AND TEMP-PART(IDX) NOT =SPACE
              DISPLAY "OTHER !!! " TEMP-PART(IDX)

           *> ============= 單純數字->M-NO(9)->郵遞區號 =============
           MOVE LENGTH OF FUNCTION TRIM(TEMP-PART(IDX)) TO TP-LEN
           MOVE "Y" TO TEMP-FLAG *> 是否為 數字/ "-"組成之字串
           PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TP-LEN
             IF NOT(TEMP-PART(IDX)(JDX:1) IS NUMERIC OR
                    TEMP-PART(IDX)(JDX:1) = "-")
                    MOVE "N" TO TEMP-FLAG
                    EXIT PERFORM
             END-IF
           END-PERFORM

           IF (TEMP-FLAG = "Y") OR
              (TEMP-PART(IDX)(1:TP-LEN - 1) IS NUMERIC AND 
               TEMP-PART(IDX)(TP-LEN:1) IS ALPHABETIC-UPPER)
               IF DTLS(9) = SPACES
                 MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(9)
               ELSE
                 MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(1)
               END-IF
           ELSE
           

           *> ====================== 郵遞區號判斷 ======================
           *> =================== （英國/加拿大格式） ===================
           IF DTLS(1) = SPACES
             UNSTRING TEMP-PART(IDX)
               DELIMITED BY SPACES
               INTO TEMP-A TEMP-B
             END-UNSTRING
             MOVE FUNCTION UPPER-CASE(DTLS(2)) TO TEMP-COL

             *> "XXX XXX" 標準寫法
             IF TEMP-B NOT = SPACES
               MOVE LENGTH OF FUNCTION TRIM(TEMP-A) TO TEMP-A-LEN
               MOVE LENGTH OF FUNCTION TRIM(TEMP-B) TO TEMP-B-LEN
               MOVE 0 TO LEFT-CNT RIGHT-CNT
               MOVE "Y" TO CNT-U TEMP-A-CNT-U TEMP-B-CNT-U
               PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TEMP-A-LEN
                 *> 數字判斷
                 IF TEMP-A(JDX:1) IS NUMERIC OR
                    TEMP-A(JDX:1) = "-"
                      ADD 1 TO TEMP-A-CNT
                 END-IF
                 *> 大寫判斷
                 IF NOT(TEMP-A(JDX:1) IS ALPHABETIC-UPPER OR
                    TEMP-A(JDX:1) IS NUMERIC OR
                    TEMP-A(JDX:1) = "-")
                      MOVE "N" TO TEMP-A-CNT-U
                      EXIT PERFORM
                 END-IF
               END-PERFORM

               PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TEMP-B-LEN
                 *> 數字判斷
                 IF TEMP-B(JDX:1) IS NUMERIC OR
                    TEMP-B(JDX:1) = "-"
                      ADD 1 TO TEMP-B-CNT
                 END-IF
                 *> 大寫判斷
                 IF NOT(TEMP-B(JDX:1) IS ALPHABETIC-UPPER OR
                    TEMP-B(JDX:1) IS NUMERIC OR
                    TEMP-B(JDX:1) = "-")
                      MOVE "N" TO TEMP-B-CNT-U
                      EXIT PERFORM
                 END-IF
               END-PERFORM

             IF (
                *> 1. 非英國 標準寫法
           (
           (TEMP-A-LEN = 3                      AND TEMP-A-CNT = 1)) AND
           (TEMP-B-LEN >= 3 AND TEMP-B-LEN <= 4 AND TEMP-B-CNT >= 1)

                OR

                *> 2. 英國 標準寫法
           ((TEMP-COL = "UNITED KINGDOM" OR TEMP-COL = "UK") AND
           ((TEMP-A-LEN >= 1 AND TEMP-A-LEN <= 2 AND TEMP-A-CNT < 2) OR
           (TEMP-A-LEN >= 3 AND TEMP-A-LEN <= 4 AND TEMP-A-CNT = 1)) AND
           (TEMP-B-LEN >= 3 AND TEMP-B-LEN <= 4 AND TEMP-B-CNT = 1))

                OR

                *> 3. 荷蘭 標準寫法
           (
           TEMP-A-LEN = 4 AND TEMP-A-CNT = 4 AND
           TEMP-B-LEN = 2 AND FUNCTION TRIM(TEMP-B) IS ALPHABETIC-UPPER)

                )

                MOVE IDX TO PR-FLAG
                MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(1)
             END-IF
             
             *> "XXXXXX" 手寫常見
             ELSE
             MOVE LENGTH OF FUNCTION TRIM(TEMP-PART(IDX)) TO TP-LEN
             MOVE "Y" TO CNT-U
             PERFORM VARYING JDX FROM 1 BY 1 UNTIL JDX > TP-LEN
               *> 數字判斷
               IF TEMP-PART(IDX)(JDX:1) IS NUMERIC OR
                  TEMP-PART(IDX)(JDX:1) = "-" OR
                  TEMP-PART(IDX)(JDX:1) = SPACE
                    ADD 1 TO CNT
               END-IF
               *> 大寫判斷
               IF NOT(TEMP-PART(IDX)(JDX:1) IS ALPHABETIC-UPPER OR
                  TEMP-PART(IDX)(JDX:1) IS NUMERIC OR
                  TEMP-PART(IDX)(JDX:1) = "-")
                    MOVE "N" TO CNT-U
                    EXIT PERFORM
               END-IF
             END-PERFORM

             IF (CNT-U = "Y" AND (
                *> 1. 非英國 手寫常見
                (TP-LEN >= 6 AND TP-LEN <= 7 AND CNT >= 2)

                OR

                *> 2. 英國 手寫常見
                ((TEMP-COL = "UNITED KINGDOM" OR TEMP-COL = "UK") AND
                ((TP-LEN >= 4 AND TP-LEN <= 6 AND CNT < 3) OR
                 (TP-LEN >= 6 AND TP-LEN <= 8 AND CNT = 2)))
                ))

                MOVE IDX TO PR-FLAG
                MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(1)
             END-IF

             END-IF
           END-IF
           *> =============== 完成 郵遞區號判斷 ===============

           *> =============== STATE -> OTHER ===============
               *> STATE欄 確認 (ZIP)
               *> 2~3大寫英文
                   IF DTLS(3) NOT = SPACES AND PR-FLAG NOT = IDX AND
                      (LENGTH OF FUNCTION TRIM(TEMP-PART(IDX)) = 2  OR 
                       LENGTH OF FUNCTION TRIM(TEMP-PART(IDX)) = 3) AND
                      FUNCTION TRIM(TEMP-PART(IDX)) IS ALPHABETIC-UPPER
                        MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(17)
                    ELSE
               *> DISTRICT欄 確認
                    IF DTLS(4) = SPACES AND PR-FLAG NOT = IDX
                        MOVE DTLS(3) TO DTLS(4)
                        MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(3)
                    ELSE
               *> STREET欄 確認
                    IF DTLS(5) = SPACES AND PR-FLAG NOT = IDX
                       MOVE DTLS(3) TO DTLS(5)
                       MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(3)
                    ELSE
               *> ZIP欄 確認
                       IF PR-FLAG NOT = IDX
                         MOVE FUNCTION TRIM(TEMP-PART(IDX)) TO DTLS(18)
                         DISPLAY "GO TO OTHER: " DTLS(18)
                       END-IF  *> ZIP欄 確認
                    END-IF     *> STREET欄 確認
                  END-IF       *> DISTRICT欄 確認
                 END-IF        *> STATE欄 確認

           END-IF
           END-IF
           END-PERFORM.


           *> ===============         特殊處理         ===============
           *> ======== 去除文字 段(6)巷(7)弄(8)號(9)樓(11)室(13) ========
           PERFORM VARYING IDX FROM 6 BY 1 UNTIL IDX > 13
           IF NOT(DTLS(IDX) = SPACES OR DTLS(IDX) IS NUMERIC OR
                  IDX = 10 OR IDX = 12)
           PERFORM VARYING JDX FROM 2 BY 1 UNTIL JDX > 40
             MOVE 0 TO CNT
             MOVE SPACES TO TEMP-A TEMP-B
             MOVE FUNCTION UPPER-CASE(DTLS(IDX)) TO TEMP-A
             MOVE FUNCTION TRIM(LS-LIST-COL(IDX JDX)) TO TEMP-B
             IF TEMP-B = SPACES
               EXIT PERFORM
             END-IF

             INSPECT TEMP-A TALLYING CNT FOR ALL FUNCTION TRIM(TEMP-B)
             IF CNT > 0
               MOVE SPACES TO TEMP-PART(1) TEMP-PART(2)
                              TEMP-PART(3) TEMP-COL
               UNSTRING TEMP-A
                 DELIMITED BY FUNCTION TRIM(TEMP-B)
                 INTO TEMP-PART(1) TEMP-PART(2) TEMP-PART(3)
               END-UNSTRING

               STRING
                 FUNCTION TRIM(TEMP-PART(1)) DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-PART(2)) DELIMITED BY SIZE
                 FUNCTION TRIM(TEMP-PART(3)) DELIMITED BY SIZE
                 INTO TEMP-COL
               END-STRING
               MOVE FUNCTION TRIM(TEMP-COL) TO DTLS(IDX)

             END-IF
           END-PERFORM
           END-IF
           END-PERFORM.

           *> 處理結束
           EXIT PROGRAM.
       END PROGRAM SPLIT-ADDRESS-FIELDS.
