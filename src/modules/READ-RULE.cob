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


      *******************************************************
      *> 資料區、WORKING-STORAGE SECTION
      *******************************************************
       WORKING-STORAGE SECTION.
       01 IDX PIC 99999 VALUE 1.
       01 WS-END-FLAG            PIC X VALUE "N".


      *******************************************************
      *> 資料區、LINKAGE SECTION
      *******************************************************
       LINKAGE SECTION.
       01 LS-LIST-REC.
           05  LS-LIST-G       OCCURS 18 TIMES.
              10  LS-LIST-COL       PIC X(35) OCCURS 40 TIMES.
           05  LS-COUNTRY-COL       PIC X(50) OCCURS 500 TIMES.
           05  LS-CITY-COL          PIC X(50) OCCURS 50000 TIMES.


      *******************************************************
      *> 程式處理區
      *******************************************************
       PROCEDURE DIVISION USING LS-LIST-REC.

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
                 MOVE FUNCTION TRIM(COUNTRY-REC) TO LS-COUNTRY-COL(IDX)
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
           *> 處理結束
           EXIT PROGRAM.
       END PROGRAM READ-RULE.
