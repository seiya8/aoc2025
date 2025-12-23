       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY05P1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.   
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 100 CHARACTERS.
       01  IN-RECORD            PIC X(100).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-EOD             PIC X VALUE "0".
         03  WK-N-RANGE         PIC 9(3) VALUE ZERO.
         03  WK-I               PIC 9(3).
         03  WK-ID              PIC 9(16).
         03  WK-ANS             PIC 9(12) COMP-3 VALUE 0.
         03  WK-FROMTO OCCURS 1000 TIMES.
             05  WK-FROM        PIC 9(16).
             05  WK-TO          PIC 9(16).
       PROCEDURE DIVISION.
       000-CONTROL.
           PERFORM 100-INIT.
           PERFORM 200-MAIN.
           PERFORM 300-END.
           STOP RUN.
       
      ******************************************************************
      * INITIALIZATION PROCESS                                         *
      ******************************************************************
       100-INIT.
      * OPEN FILE
           OPEN INPUT INPUT-FILE.
      * READ ALL LINES BEFORE THE BLANK LINE
           PERFORM UNTIL WK-EOD = "1"
               READ INPUT-FILE
                 AT END
                   MOVE "1" TO WK-EOD
                 NOT AT END
                   IF IN-RECORD = SPACES
                      EXIT PERFORM
                   END-IF
                   ADD 1 TO WK-N-RANGE
                   UNSTRING IN-RECORD DELIMITED BY "-"
                     INTO  WK-FROM(WK-N-RANGE)
                           WK-TO(WK-N-RANGE)
               END-READ
           END-PERFORM.

      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
      * READ ALL LINES AFTER THE BLANK LINE
           PERFORM UNTIL WK-EOD = "1"
               READ INPUT-FILE
                 AT END
                   MOVE "1" TO WK-EOD
                 NOT AT END
      * CHECK FOR ALL THE FRESH ID RANGES
                   PERFORM VARYING WK-I FROM 1 BY 1
                     UNTIL WK-I > WK-N-RANGE
                       MOVE IN-RECORD TO WK-ID
                       IF WK-ID >= WK-FROM(WK-I) AND
                          WK-ID <= WK-TO(WK-I)
                           ADD 1 TO WK-ANS
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-READ
           END-PERFORM.

      ******************************************************************
      * END PROCESS                                                    *
      ******************************************************************
       300-END.
      * CLOSE FILE
           CLOSE INPUT-FILE.
      * DISPLAY ANSWER
           DISPLAY WK-ANS.
