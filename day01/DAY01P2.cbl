       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 4 CHARACTERS.
       01  IN-RECORD.
         03  IN-DIR             PIC X(1).
         03  IN-DIS             PIC X(3).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-EOF             PIC X(1) VALUE "0".
         03  WK-DIR-SIGN        PIC S9(1).
         03  WK-DIS             PIC 9(3).
         03  WK-POS             PIC S9(4) VALUE 50.
         03  WK-DIS-TO-ORG      PIC 9(3).
         03  WK-PWD             PIC 9(5) VALUE ZERO.
       PROCEDURE DIVISION.
       000-CONTROL.
           PERFORM 100-INIT.
           PERFORM 200-MAIN UNTIL WK-EOF = "1".
           PERFORM 300-END.
           STOP RUN.
       
      ******************************************************************
      * INITIALIZATION PROCESS                                         *
      ******************************************************************
       100-INIT.
      * OPEN FILE
           OPEN INPUT INPUT-FILE.
      * READ FIRST LINE
           READ INPUT-FILE
               AT END MOVE "1" TO WK-EOF
           END-READ.
       
      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
           MOVE IN-DIS TO WK-DIS.
           IF IN-DIR = "R"
               MOVE -1 TO WK-DIR-SIGN
           ELSE
               MOVE 1 TO WK-DIR-SIGN
           END-IF.
      * COMPUTE MINIMUM DISTANCE TO ORIGIN
           COMPUTE WK-DIS-TO-ORG = WK-DIR-SIGN * WK-POS + 100.
           COMPUTE WK-DIS-TO-ORG = FUNCTION MOD(WK-DIS-TO-ORG 100).
           IF WK-DIS-TO-ORG = 0
               MOVE 100 TO WK-DIS-TO-ORG
           END-IF.
      * ADD TIMES OF ZERO-CROSSING TO PASSWORD
           IF WK-DIS >= WK-DIS-TO-ORG
               COMPUTE WK-PWD
                 = WK-PWD + (WK-DIS - WK-DIS-TO-ORG) / 100 + 1
           END-IF.
      * UPDATE POSITION
           COMPUTE WK-POS
             = WK-POS - WK-DIR-SIGN * FUNCTION MOD(WK-DIS 100).
           COMPUTE WK-POS = FUNCTION MOD(WK-POS 100).
      * READ NEXT LINE
           READ INPUT-FILE
               AT END MOVE "1" TO WK-EOF
           END-READ.

      ******************************************************************
      * END PROCESS                                                    *
      ******************************************************************
       300-END.
      * CLOSE FILE
           CLOSE INPUT-FILE.
      * DISPLAY ANSWER
           DISPLAY WK-PWD.
