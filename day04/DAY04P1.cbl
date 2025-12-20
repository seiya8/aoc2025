       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY03P1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT"
               ORGANIZATION IS LINE SEQUENTIAL.   
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 140 CHARACTERS.
       01  IN-RECORD            PIC X(140).
       WORKING-STORAGE SECTION.
       01  WK-AREA.
         03  WK-I               PIC 9(3).
         03  WK-J               PIC 9(3).
         03  WK-K               PIC 9.
         03  WK-CNT             PIC 9.
         03  WK-GRID OCCURS 142 TIMES.
           05  WK-ROW           PIC X(142) VALUE ".".
         03  WK-ANS             PIC 9(6) VALUE 0.
         03  WK-DIR-TABLE OCCURS 8 TIMES.
             05  DIR-I          PIC S9 VALUE ZERO.
             05  DIR-J          PIC S9 VALUE ZERO.
       01  CON-AREA.
         03  CON-DOTS           PIC X(142) VALUE ALL ".".
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
      * INITIALIZE DIRECTION TABLE
           PERFORM 110-INIT-TABLE.
      * INITIALIZE FIRST AND LAST ROWS OF THE PADDED GRID
           MOVE CON-DOTS TO WK-ROW(1).
           MOVE CON-DOTS TO WK-ROW(142).
      * OPEN FILE
           OPEN INPUT INPUT-FILE.
      * READ ALL LINE
           PERFORM VARYING WK-I FROM 2 BY 1 UNTIL WK-I > 141
               READ INPUT-FILE
               MOVE IN-RECORD TO WK-ROW(WK-I)(2:140)
               MOVE "." TO WK-ROW(WK-I)(142:1)
           END-PERFORM.
      * CLOSE FILE
           CLOSE INPUT-FILE.

      ******************************************************************
      * INITIALIZE DIRECTION TABLE PROCESS                             *
      ******************************************************************
       110-INIT-TABLE.
           MOVE -1 TO DIR-I(1) DIR-I(2) DIR-I(3) 
                      DIR-J(1) DIR-J(4) DIR-J(6)
           MOVE  0 TO DIR-I(4) DIR-I(5) DIR-J(2) DIR-J(7).
           MOVE  1 TO DIR-I(6) DIR-I(7) DIR-I(8) 
                      DIR-J(3) DIR-J(5) DIR-J(8).

      ******************************************************************
      * MAIN PROCESS                                                   *
      ******************************************************************
       200-MAIN.
           PERFORM VARYING WK-I FROM 2 BY 1 UNTIL WK-I > 141
               PERFORM VARYING WK-J FROM 2 BY 1 UNTIL WK-J > 141
                   IF WK-ROW(WK-I)(WK-J:1) = "@"
                     THEN
                       MOVE ZERO TO WK-CNT
                       PERFORM 210-CHECK
                       IF WK-CNT < 4
                         THEN
                           ADD 1 TO WK-ANS
                       END-IF    
                   END-IF
               END-PERFORM
           END-PERFORM.

      ******************************************************************
      * CHECK PROCESS                                                  *
      ******************************************************************
       210-CHECK.
           PERFORM VARYING WK-K FROM 1 BY 1 UNTIL WK-K > 8
               IF WK-ROW(WK-I+DIR-I(WK-K))(WK-J+DIR-J(WK-K):1) = "@"
                 THEN
                   ADD 1 TO WK-CNT
               END-IF
           END-PERFORM.

      ******************************************************************
      * END PROCESS                                                    *
      ******************************************************************
       300-END.
      * DISPLAY ANSWER
           DISPLAY WK-ANS.
