      *> PS C:\vs-workspaces\Cobol-Test> cobc -x -I src\main -I src\copy 
      *> -o bin\test.exe src\main\test.cob
      
      *> PS C:\vs-workspaces\Cobol-Test> bin\test.exe

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY COPYDAT.

       PROCEDURE DIVISION.
       MIN-PROCEDURE.
           MOVE "ABCD" TO FILE-DATA-X
           DISPLAY FILE-DATA-X
           STOP RUN.    
