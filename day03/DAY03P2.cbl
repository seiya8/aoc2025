       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY03P2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 100 CHARACTERS.
       01  IN-RECORD           PIC X(100).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-EOF             PIC X(1) VALUE "0".
         03  WK-I               PIC 9.
         03  WK-J               PIC 9(3).
         03  WK-POS             PIC 9(3).
         03  WK-MAX-CHR         PIC X.
         03  WK-NUM             PIC X(12).
         03  WK-ANS             PIC 9(14) VALUE 0.
       01  CON-AREA.
         03  CON-DIGIT          PIC 9(2) VALUE 12.
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
      * INITIALIZATION
           MOVE 1 TO WK-POS.
      * LOOP FOR I-TH DIGIT (FROM LEFT)
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > CON-DIGIT
               MOVE "0" TO WK-MAX-CHR
      * LOOP TO FIND MAX VALUE FOR I-TH DIGIT
               PERFORM VARYING WK-J FROM WK-POS BY 1
                 UNTIL WK-J > 100 - (CON-DIGIT - WK-I)
                   IF IN-RECORD(WK-J:1) > WK-MAX-CHR
                       MOVE WK-J TO WK-POS
                   END-IF
               END-PERFORM
      * INSERT INTO I-TH DIGIT
               MOVE WK-MAX-CHR TO WK-NUM(WK-I:1)
      * FOR I+1-TH DIGIT, SEARCH FROM NEXT POSITION
               ADD 1 TO WK-POS
           END-PERFORM.
      * UPDATE ANSWER
           COMPUTE WK-ANS = WK-ANS + FUNCTION NUMVAL(WK-NUM).
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
           DISPLAY WK-ANS.
