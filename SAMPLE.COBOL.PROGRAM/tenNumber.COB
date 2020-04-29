      ******************************************************************
      * Author: SANDEEP PRJAPATI
      * Date: 18-04-2020
      * Purpose: RANDOM NUMBER GENERATION
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEN-NUM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WORK     PIC  9(7)V9(7) VALUE 0.
       77  SEED     PIC V9(7)      VALUE .1.
       77  WS-COUNT PIC  999       VALUE 0.
       77  S1       PIC  X         VALUE '/'.
       77  S2       PIC  X         VALUE '\'.
       77  WS-LEFT  PIC  999       VALUE 0.
       77  WS-RIGHT PIC  999       VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            PERFORM COUNT-LOOP UNTIL WS-COUNT IS EQUAL TO 999.
            DISPLAY WS-LEFT "+" WS-RIGHT "+" WS-COUNT.
            STOP RUN.

           COUNT-LOOP.
                PERFORM RNG
            IF SEED IS LESS THAN .5
                DISPLAY S1
                ADD 1 TO WS-LEFT
            ELSE
               DISPLAY S2
               ADD 1 TO WS-RIGHT.
            ADD 1 TO WS-COUNT.

           RNG.
               MULTIPLY SEED BY 214013 GIVING WORK.
               ADD WORK .2531011 GIVING WORK.
               MOVE WORK TO SEED.
