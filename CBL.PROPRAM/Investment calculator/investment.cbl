      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 06-05-2020
      * Purpose: INTERACTIVE COBOL PROGRAM TO CALCULATE
      *        : FUTURE VALUE ON INVESTMENT AMOUNT
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CALC1001.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01  USER-ENTERED.
           05 INVESTMENT-AMOUNT      PIC 99999.
           05 NO-OF-YEARS            PIC 99.
           05 NUMBER-ENTERED         PIC 9             VALUE 1.
           05 YEARLY-INTEREST-RATE   PIC 99V9.
      *
       01  WORK-FIELDS.
           05 FUTURE-VALUE           PIC 9(7)V99.
           05 YEAR-COUNTER           PIC 999.
           05 EDITED-FUTURE-VALUE    PIC Z,ZZZ,ZZZ.99.
      *
       PROCEDURE DIVISION.
       000-CALCULATE-FUTURE-VALUE.
      *
            DISPLAY "HELLO WORLD!!! - THIS IS"
            DISPLAY "INTERACTIVE COBOL PROGRAM TO CALCULATE".
            DISPLAY "FUTURE VALUE ON INVESTMENT AMOUNT".
      *
            PERFORM 100-CALCULATE-FUTURE-VALUE
               UNTIL NUMBER-ENTERED = 0.
      *
            DISPLAY "END OF SESSION".
      *
            STOP RUN.
      *
       100-CALCULATE-FUTURE-VALUE.
      *
            DISPLAY "-------------------------------------------------".
            DISPLAY "TO END THE PROGRAM, ENTER 0.".
            DISPLAY "TO PERFORM ANOTHER CALCUALTION, ENTER 1.".
            ACCEPT NUMBER-ENTERED.
            DISPLAY "-------------------------------------------------".
      *
               IF NUMBER-ENTERED = 1
                   PERFORM 110-GET-USER-VALUES
                   MOVE INVESTMENT-AMOUNT TO FUTURE-VALUE
                   MOVE 1 TO YEAR-COUNTER
                   PERFORM 120-CALCULATE-NXT-FV
                       UNTIL YEAR-COUNTER > NO-OF-YEARS
                   MOVE FUTURE-VALUE TO EDITED-FUTURE-VALUE
                   DISPLAY "FUTURE VALUE = " EDITED-FUTURE-VALUE.
      *
       110-GET-USER-VALUES.
      *
               DISPLAY 'ENTER INVESTMENT AMOUNT (XXXXX).'.
               ACCEPT INVESTMENT-AMOUNT.
               DISPLAY 'ENTER NUMBER OF YEAR (XX).'.
               ACCEPT NO-OF-YEARS.
               DISPLAY 'ENTER YEARLY INTEREST RATE (XX.X).'.
               ACCEPT YEARLY-INTEREST-RATE.
      *
       120-CALCULATE-NXT-FV.
      *
               COMPUTE FUTURE-VALUE ROUNDED =
                   FUTURE-VALUE +
                       (FUTURE-VALUE * YEARLY-INTEREST-RATE)/100.
               ADD 1 TO YEAR-COUNTER.
      *
       END PROGRAM CALC1001.
