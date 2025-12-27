       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY05P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.   
           SELECT SORT-FILE ASSIGN TO "SORT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 100 CHARACTERS.
       01  IN-RECORD            PIC X(100).
       SD  SORT-FILE.
       01  SORT-REC.
         03  SORT-FROM          PIC 9(16).
         03  SORT-TO            PIC 9(16).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-EOD             PIC X VALUE "0".
         03  WK-N-RANGE         PIC 9(3) VALUE ZERO.
         03  WK-I               PIC 9(3).
         03  WK-J               PIC 9(3).
         03  WK-J-INI           PIC 9(3).
         03  WK-TMP-FROM        PIC 9(16).
         03  WK-TMP-TO          PIC 9(16).
         03  WK-CUR-FROM        PIC 9(16).
         03  WK-CUR-TO          PIC 9(16).
         03  WK-ANS             PIC 9(20) COMP-3 VALUE 0.
         03  WK-FROMTO OCCURS 200 TIMES.
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
      * CLOSE FILE
           CLOSE INPUT-FILE.

      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
      * SORT FILE BY WK-FROM
           PERFORM 210-SORT.
      * COUNT FRESH ID
           PERFORM 220-COUNT.

      ******************************************************************
      * BUBBLE SORT                                                    *
      ******************************************************************
       210-SORT.
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > WK-N-RANGE
               COMPUTE WK-J-INI = WK-I + 1
               PERFORM VARYING WK-J FROM WK-J-INI BY 1
                 UNTIL WK-J > WK-N-RANGE
                   IF WK-FROM(WK-I) > WK-FROM(WK-J)
                       PERFORM 211-SWAP
                   END-IF
               END-PERFORM
           END-PERFORM.

      ******************************************************************
      * SWAP TWO RECORDS IN BUBBLE SORT                                *
      ******************************************************************
       211-SWAP.
           MOVE WK-FROM(WK-I) TO WK-TMP-FROM.
           MOVE WK-TO(WK-I) TO WK-TMP-TO.

           MOVE WK-FROM(WK-J) TO WK-FROM(WK-I).
           MOVE WK-TO(WK-J) TO WK-TO(WK-I).

           MOVE WK-TMP-FROM TO WK-FROM(WK-J).
           MOVE WK-TMP-TO TO WK-TO(WK-J).

      ******************************************************************
      * COUNT FRESH IDS                                                *
      ******************************************************************
       220-COUNT.
      * SAVE THE FIRST RECORD
           MOVE WK-FROM(1) TO WK-CUR-FROM.
           MOVE WK-TO(1) TO WK-CUR-TO.
           PERFORM VARYING WK-I FROM 2 BY 1 UNTIL WK-I > WK-N-RANGE
      * IF OVERLAPPED UPDATE WK-TO
               IF WK-FROM(WK-I) <= WK-CUR-TO + 1
                   IF WK-TO(WK-I) > WK-CUR-TO
                       MOVE WK-TO(WK-I) TO WK-CUR-TO
                   END-IF
               ELSE
      * IF NOT OVERLAPPED UPDATE WK-ANS
                   COMPUTE WK-ANS =
                     WK-ANS + (WK-CUR-TO - WK-CUR-FROM + 1)
      * SAVE THE CURRENT RANGE
                   MOVE WK-FROM(WK-I) TO WK-CUR-FROM
                   MOVE WK-TO(WK-I) TO WK-CUR-TO
               END-IF
           END-PERFORM.
      * ADD THE LAST FRESH RANGE
           COMPUTE WK-ANS = WK-ANS + (WK-CUR-TO - WK-CUR-FROM + 1).

      ******************************************************************
      * END PROCESS                                                    *
      ******************************************************************
       300-END.
      * DISPLAY ANSWER
           DISPLAY WK-ANS.
