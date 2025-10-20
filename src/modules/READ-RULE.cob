       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-RULE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIST-FILE ASSIGN
              TO "input\CategoryRules.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COUNTRY-FILE ASSIGN
              TO "input\CountryList.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CITY-FILE ASSIGN
              TO "input\WorldCitiesList.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STATE-FILE ASSIGN
              TO "input\StateFullnameList.csv"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
      *******************************************************
      *> 資料區、FILE SECTION
      *******************************************************
       FILE SECTION.
       FD  LIST-FILE
           RECORD CONTAINS 210 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 LIST-REC PIC X(210).

       FD  COUNTRY-FILE
           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 COUNTRY-REC PIC X(50).

       FD  CITY-FILE
           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 CITY-REC PIC X(50).

       FD  STATE-FILE
           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 STATE-REC PIC X(60).


      *******************************************************
      *> 資料區、WORKING-STORAGE SECTION
      *******************************************************
       WORKING-STORAGE SECTION.
       01 IDX PIC 99999 VALUE 1.
       01 WS-END-FLAG            PIC X VALUE "N".
       01 ST-DATA                PIC X(60) OCCURS 3 TIMES.
       01 TEMP-COL               PIC X(40).


      *******************************************************
      *> 資料區、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES.
           05  LS-STATE-NAME-COL    PIC X(45) OCCURS 200 TIMES.
           05  LS-STATE-CODE-COL    PIC X(10) OCCURS 200 TIMES.
           05  DIR-NAMES OCCURS 21 TIMES PIC X(8). *> 全方向


      *******************************************************
      *> 程式處理區
      *******************************************************
       PROCEDURE DIVISION USING LS-LIST-REC.

           *> DIR-NAMES 初期化
           MOVE "NORTH"   TO DIR-NAMES(1).
           MOVE "SOUTH"   TO DIR-NAMES(2).
           MOVE "EAST"    TO DIR-NAMES(3).
           MOVE "WEST"    TO DIR-NAMES(4).
           MOVE "NE"      TO DIR-NAMES(5).
           MOVE "NW"      TO DIR-NAMES(6).
           MOVE "SE"      TO DIR-NAMES(7).
           MOVE "SW"      TO DIR-NAMES(8).
           MOVE "N"       TO DIR-NAMES(9).
           MOVE "S"       TO DIR-NAMES(10).
           MOVE "E"       TO DIR-NAMES(11).
           MOVE "W"       TO DIR-NAMES(12).
           MOVE "N."      TO DIR-NAMES(13).
           MOVE "S."      TO DIR-NAMES(14).
           MOVE "E."      TO DIR-NAMES(15).
           MOVE "W."      TO DIR-NAMES(16).
           MOVE "KITA"    TO DIR-NAMES(17).
           MOVE "MINAMI"  TO DIR-NAMES(18).
           MOVE "HIGASHI" TO DIR-NAMES(19).
           MOVE "NISHI"   TO DIR-NAMES(20).
           MOVE "LOOP"    TO DIR-NAMES(21).

      *******************************************************
      *> LIST.csv 讀取
      *******************************************************
           OPEN INPUT LIST-FILE.

           PERFORM UNTIL WS-END-FLAG = "Y"
              READ LIST-FILE
                 AT END
                    MOVE "Y" TO WS-END-FLAG
                 NOT AT END
                 *> 儲存各項目
                  UNSTRING LIST-REC
                      DELIMITED BY ","
                      INTO LS-LIST-COL (IDX 1)
                           LS-LIST-COL (IDX 2)
                           LS-LIST-COL (IDX 3)
                           LS-LIST-COL (IDX 4)
                           LS-LIST-COL (IDX 5)
                           LS-LIST-COL (IDX 6)
                           LS-LIST-COL (IDX 7)
                           LS-LIST-COL (IDX 8)
                           LS-LIST-COL (IDX 9)
                           LS-LIST-COL (IDX 10)
                           LS-LIST-COL (IDX 11)
                           LS-LIST-COL (IDX 12)
                           LS-LIST-COL (IDX 13)
                           LS-LIST-COL (IDX 14)
                           LS-LIST-COL (IDX 15)
                           LS-LIST-COL (IDX 16)
                           LS-LIST-COL (IDX 17)
                           LS-LIST-COL (IDX 18)
                           LS-LIST-COL (IDX 19)
                           LS-LIST-COL (IDX 20)
                           LS-LIST-COL (IDX 21)
                           LS-LIST-COL (IDX 22)
                           LS-LIST-COL (IDX 23)
                           LS-LIST-COL (IDX 24)
                           LS-LIST-COL (IDX 25)
                           LS-LIST-COL (IDX 26)
                           LS-LIST-COL (IDX 27)
                           LS-LIST-COL (IDX 28)
                           LS-LIST-COL (IDX 29)
                           LS-LIST-COL (IDX 30)
                           LS-LIST-COL (IDX 31)
                           LS-LIST-COL (IDX 32)
                           LS-LIST-COL (IDX 33)
                           LS-LIST-COL (IDX 34)
                           LS-LIST-COL (IDX 35)
                           LS-LIST-COL (IDX 36)
                           LS-LIST-COL (IDX 37)
                           LS-LIST-COL (IDX 38)
                           LS-LIST-COL (IDX 39)
                           LS-LIST-COL (IDX 40)
                   ADD 1 TO IDX
              END-READ
           END-PERFORM.

           CLOSE LIST-FILE.

      *******************************************************
      *> CountryList.csv 讀取
      *******************************************************
           MOVE "N" TO WS-END-FLAG.
           MOVE 1   TO IDX.
           OPEN INPUT COUNTRY-FILE.

           PERFORM UNTIL WS-END-FLAG = "Y"
             READ COUNTRY-FILE
               AT END
                 MOVE "Y" TO WS-END-FLAG
               NOT AT END
                 *> 移除 CSV 讀取字串時可能產生的多餘字串 '"'
                 MOVE FUNCTION TRIM(COUNTRY-REC) TO TEMP-COL
                 IF TEMP-COL(1:1) = '"'
                   MOVE TEMP-COL(1: 
                     LENGTH OF FUNCTION TRIM(TEMP-COL) - 2) TO
                     LS-COUNTRY-COL(IDX)
                 ELSE
                   MOVE TEMP-COL TO LS-COUNTRY-COL(IDX)
                 END-IF
                 ADD 1 TO IDX
             END-READ
           END-PERFORM.

           CLOSE COUNTRY-FILE.

      *******************************************************
      *> WorldCitiesList.csv 讀取
      *******************************************************
           MOVE "N" TO WS-END-FLAG.
           MOVE 1   TO IDX.
           OPEN INPUT CITY-FILE.

           PERFORM UNTIL WS-END-FLAG = "Y"
             READ CITY-FILE
               AT END
                 MOVE "Y" TO WS-END-FLAG
               NOT AT END
                 MOVE FUNCTION TRIM(CITY-REC) TO LS-CITY-COL(IDX)
                 ADD 1 TO IDX
             END-READ
           END-PERFORM.

           CLOSE CITY-FILE.

      *******************************************************
      *> StateFullnameList.csv 讀取
      *******************************************************
           MOVE "N" TO WS-END-FLAG.
           MOVE 1   TO IDX.
           OPEN INPUT STATE-FILE.

           PERFORM UNTIL WS-END-FLAG = "Y"
             READ STATE-FILE
               AT END
                 MOVE "Y" TO WS-END-FLAG
               NOT AT END
                 UNSTRING STATE-REC DELIMITED BY ";"
                   INTO ST-DATA(1) ST-DATA(2) ST-DATA(3)

                 MOVE FUNCTION TRIM(ST-DATA(1))
                      TO LS-STATE-NAME-COL(IDX)
                 MOVE FUNCTION TRIM(ST-DATA(2))
                      TO LS-STATE-CODE-COL(IDX)
                 ADD 1 TO IDX
             END-READ
           END-PERFORM.

           CLOSE STATE-FILE.


           *> 處理結束
           EXIT PROGRAM.
       END PROGRAM READ-RULE.
