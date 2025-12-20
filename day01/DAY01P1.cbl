       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01P1.

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
         03  WK-DIS             PIC 9(3).
         03  WK-POS             PIC 9(4) VALUE 50.
         03  WK-PWD             PIC 9(4) VALUE ZERO.

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
      * CONSIDER LEFT ROTATION AS RIGHT ROTATION
           IF IN-DIR = "L"
             THEN
               COMPUTE WK-DIS = 1000 - WK-DIS
           END-IF.
      * UPDATE POSITION
           ADD WK-DIS TO WK-POS.
           COMPUTE WK-POS = FUNCTION MOD(WK-POS 100).
      * INCREMENT PASSWORD
           IF WK-POS = ZERO
             THEN
               ADD 1 TO WK-PWD
           END-IF.
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
