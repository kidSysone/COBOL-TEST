       IDENTIFICATION DIVISION.       *> プログラムの基本情報を定義する場所
       PROGRAM-ID. TESTFILE.          *> このプログラムの名前は TESTFILE

       ENVIRONMENT DIVISION.          *> 環境設定を記述する場所
       INPUT-OUTPUT SECTION.          *> ファイル入出力の設定をする場所
       FILE-CONTROL.                  *> 実際に使うファイルの指定
           SELECT INFILE ASSIGN TO "C:\Cobol\input\CobolTest_A.txt"
              ORGANIZATION IS LINE SEQUENTIAL. *> INFILE を行単位で読み込む
           SELECT OUTFILE ASSIGN TO "C:\Cobol\output\CobolTest_B.txt"
              ORGANIZATION IS LINE SEQUENTIAL. *> OUTFILE を行単位で書き込む

       DATA DIVISION.                  *> データを定義する場所
       FILE SECTION.                  *> ファイルごとのレコード定義
       FD  INFILE.                    *> INFILE の定義開始
       01  IN-REC        PIC X(100). *> 1行最大100文字の文字列として定義

       FD  OUTFILE.                   *> OUTFILE の定義開始
       01  OUT-REC       PIC X(100). *> 1行最大100文字の文字列として定義

       WORKING-STORAGE SECTION.       *> プログラム内で使う作業用変数の定義
       01  EOF-FLAG      PIC X VALUE "N". *> ファイル終端(EOF)かどうかのフラグ、初期値は N

       PROCEDURE DIVISION.            *> 実際の処理を書く場所
       MAIN-PARA.                     *> 段落名（処理のまとまり）

           OPEN INPUT INFILE
                OUTPUT OUTFILE       *> INFILE を読み込み用、OUTFILE を書き込み用にオープン

           PERFORM UNTIL EOF-FLAG = "Y"  *> EOF-FLAG が Y になるまで繰り返す
              READ INFILE               *> INFILE から 1 行読み込む
                 AT END
                    MOVE "Y" TO EOF-FLAG *> ファイルの終わりなら EOF-FLAG を Y にする
                 NOT AT END
                    DISPLAY "READ LINE: " IN-REC
                                         *> 読み込んだ行を画面に表示
                    MOVE IN-REC TO OUT-REC
                                         *> INFILE の内容を OUTFILE 用にコピー
                    WRITE OUT-REC       *> OUTFILE に書き込む
              END-READ
           END-PERFORM

           CLOSE INFILE OUTFILE         *> ファイルを閉じる
           STOP RUN.                     *> プログラム終了
