       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY09P1.
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
         03  WK-I               PIC 9(3).
         03  WK-J               PIC 9(3).
         03  WK-J-INI           PIC 9(3).
         03  WK-N-POINTS        PIC 9(5) VALUE ZERO.
         03  WK-SIZE            PIC 9(12) COMP-3.
         03  WK-ANS             PIC 9(12) COMP-3 VALUE 0.
         03  WK-XY OCCURS 10000 TIMES.
             05  WK-X           PIC 9(5) COMP-3.
             05  WK-Y           PIC 9(5) COMP-3.
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
      * READ ALL LINE
           PERFORM UNTIL WK-EOD = "1"
               READ INPUT-FILE
                 AT END
                   MOVE "1" TO WK-EOD
                 NOT AT END
                   ADD 1 TO WK-N-POINTS
                   UNSTRING IN-RECORD DELIMITED BY ","
                     INTO WK-X(WK-N-POINTS)
                          WK-Y(WK-N-POINTS)
               END-READ
           END-PERFORM.
      * CLOSE FILE
           CLOSE INPUT-FILE.

      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > WK-N-POINTS
               ADD 1 TO WK-J-INI
               PERFORM VARYING WK-J FROM WK-J-INI BY 1
                 UNTIL WK-J > WK-N-POINTS
                   COMPUTE WK-SIZE
                     = FUNCTION ABS(1 + WK-X(WK-I) - WK-X(WK-J))
                       * FUNCTION ABS(1 + WK-Y(WK-I) - WK-Y(WK-J))
                   IF WK-SIZE > WK-ANS
                       MOVE WK-SIZE TO WK-ANS
                   END-IF
               END-PERFORM
           END-PERFORM.

      ******************************************************************
      * END PROCESS                                                    *
      ******************************************************************
       300-END.
      * DISPLAY ANSWER
           DISPLAY WK-ANS.
