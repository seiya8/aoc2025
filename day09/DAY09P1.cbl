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
           RECORD CONTAINS 11 CHARACTERS.
       01  IN-RECORD            PIC X(11).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-I               PIC 9(3).
         03  WK-J               PIC 9(3).
         03  WK-J-INI           PIC 9(3).
         03  WK-SIZE            PIC 9(12) COMP-3.
         03  WK-ANS             PIC 9(12) COMP-3 VALUE 0.
         03  WK-XY OCCURS 496 TIMES.
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
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > 496
               READ INPUT-FILE
               UNSTRING IN-RECORD DELIMITED BY "," 
                 INTO WK-X(WK-I) WK-Y(WK-I)
           END-PERFORM.
      * CLOSE FILE
           CLOSE INPUT-FILE.

      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > 496
               ADD 1 TO WK-J-INI
               PERFORM VARYING WK-J FROM WK-J-INI BY 1 UNTIL WK-J > 496
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
