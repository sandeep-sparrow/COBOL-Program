      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 27-04-2020
      * Purpose: TEST PROGRAM
      * Tectonics: CODC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTRUN.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  I     PIC 9(01) VALUE ZEROES.
       01  N     PIC 9(01) VALUE ZEROES.
       01  WEEK.
           02 DAY-NAME  PIC X(10) OCCURS 7 TIMES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
            DISPLAY "Hello world".
            INITIALIZE I.
      *
            MOVE 'MONDAY'    TO DAY-NAME(1).
            MOVE 'TUESDAY'   TO DAY-NAME(2).
            MOVE 'WEDNESDAY' TO DAY-NAME(3).
            MOVE 'THURSDAY'  TO DAY-NAME(4).
            MOVE 'FRIDAY'    TO DAY-NAME(5).
            MOVE 'SATURDAY'  TO DAY-NAME(6).
            MOVE 'SUNDAY'    TO DAY-NAME(7).
      *
            PERFORM 100-DISPLAY-HELLO
               VARYING I FROM 1 BY 1 UNTIL I > 7.
               DISPLAY DAY-NAME(5).
      *
            STOP RUN.
      *
           100-DISPLAY-HELLO.
                DISPLAY DAY-NAME(I).
      *
       END PROGRAM TESTRUN.
