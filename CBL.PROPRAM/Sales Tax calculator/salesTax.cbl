      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 05-05-2020
      * Purpose: INTERACTIVE COBOL PROGRAM
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CALC1000.
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
       77  END-OF-SESSION-SWITCH    PIC X             VALUE 'N'.
       77  SALES-AMOUNT             PIC 9(8)V99.
       77  SALES-CHANGE             PIC ZZZ,ZZZ.99.
       77  I                        PIC 9 VALUE 1.
       01  SALES-YEAR.
           05 AMOUNT                PIC 9(8)V99 OCCURS 2 TIMES.
       77  SALES-TAX                PIC ZZZ,ZZZ.99.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
       000-CALCULATE-SALES-TAX.
      *
            DISPLAY "Hello world!"
      *
            PERFORM 100-CALCULATE-ONE-SALES-TAX
               UNTIL END-OF-SESSION-SWITCH = 'Y'.
      *
            DISPLAY "END OF SESSION".
            STOP RUN.
      *
       100-CALCULATE-ONE-SALES-TAX.
      *
            DISPLAY "-------------------------------------------------".
            DISPLAY "TO END SESSION, ENTER 0.".
            DISPLAY "TO CALCULATE SALES TAX, ENTER THE SALES AMOUNT.".
            ACCEPT SALES-AMOUNT.
            IF SALES-AMOUNT = ZERO
                MOVE 'Y' TO END-OF-SESSION-SWITCH
                DISPLAY SALES-YEAR
            ELSE
                MOVE SALES-AMOUNT TO AMOUNT(I)
                COMPUTE SALES-TAX ROUNDED = SALES-AMOUNT * 0.0785
                DISPLAY "SALES TAX = " SALES-TAX
                IF I = '2'
                    COMPUTE SALES-CHANGE = AMOUNT(1) - AMOUNT(2)
                    DISPLAY "CHANGE SALE IS = " SALES-CHANGE
                    MOVE 0 TO I
                END-IF
                ADD 1 TO I
            END-IF.


       END PROGRAM CALC1000.
