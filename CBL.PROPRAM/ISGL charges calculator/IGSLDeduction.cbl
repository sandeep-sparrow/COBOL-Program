      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 07-05-2020
      * Purpose: THIS IS ILLUSTRATION OF INTERACTIVE COBOL PROGRAM
      *        : WHICH ACCEPTS VALUES FROM USER TO CALCULATE DEDUCTIONS
      *        : FOR FINAL VALUE.
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CALC1002.
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
           05 NUMBER-ENTERED          PIC 9(01)          VALUE 1.
           05 MTM-AMOUNT              PIC 9(07)V99.
           05 BUY-AMOUNT              PIC 9(08)V99.
      *
       01  IGSL-DEDUCTIONS.
           05 BROKERAGE-CHARGE        PIC 9(06)V99.
           05 TRANSACTION-CHARGE      PIC 9(06)V99.
           05 STT-CHARGE              PIC 9(06)V99.
           05 STAMP-DUTY              PIC 9(01)          VALUE 5.
           05 GST-CHARGE              PIC 9(06)V99.
      *
       01  WORK-FIELD.
           05 TOTAL-CHARGE            PIC 9(06)V99.
           05 TOTAL-CHARGE-EDITED     PIC Z,ZZZ,ZZZ.99.
           05 MTM-AMOUNT-EDITED       PIC Z,ZZZ,ZZZ.99.
           05 BUY-AMOUNT-EDITED       PIC Z,ZZZ,ZZZ.99.
      *
       PROCEDURE DIVISION.
      *
       001-CALCULATE-TOTAL-DEDUCTION.
      *
            DISPLAY "-------------------------------------------------".
            DISPLAY "******** INVENUTRE GROWTH AND SECURITIES ********".
            DISPLAY "-------------------------------------------------".
            DISPLAY "HELLO WORDLD - THIS PROGRAM CALCULATES DEDUCTIONS".
      *
             PERFORM 100-CALCULATE-TOTAL-DEDUCTION
               UNTIL NUMBER-ENTERED =0.
      *
            DISPLAY "END OF SESSION".
      *
            STOP RUN.
      *
        100-CALCULATE-TOTAL-DEDUCTION.
      *
            DISPLAY "-------------------------------------------------".
            DISPLAY "TO END THE PROGRAM, ENTER 0.".
            DISPLAY "TO PERFORM ANOTHER CALCUALTION, ENTER 1.".
            ACCEPT NUMBER-ENTERED.
            DISPLAY "-------------------------------------------------".
      *
                   IF NUMBER-ENTERED = 1
                       PERFORM 110-GET-USER-VALUES
                       PERFORM 120-CALCULATE-NXT
      *
                       MOVE TOTAL-CHARGE TO TOTAL-CHARGE-EDITED
                       MOVE MTM-AMOUNT TO MTM-AMOUNT-EDITED
                       MOVE BUY-AMOUNT TO BUY-AMOUNT-EDITED
      *
            DISPLAY "TOTAL BUY AMOUNT = " BUY-AMOUNT-EDITED
            DISPLAY "TOTAL CHARGES = " TOTAL-CHARGE-EDITED
            DISPLAY "MTM AMOUNT AFTER DEDUCTION = " MTM-AMOUNT-EDITED
      *
                   END-IF.
      *
       110-GET-USER-VALUES.
      *
               DISPLAY 'ENTER MTM AMOUNT (XXXXXXX.XX).'.
               ACCEPT MTM-AMOUNT.
               DISPLAY 'ENTER BUY AMOUNT (XXXXXXXX.XX).'.
               ACCEPT BUY-AMOUNT.
      *
       120-CALCULATE-NXT.
      *
               COMPUTE BROKERAGE-CHARGE = BUY-AMOUNT * 0.0005.
               COMPUTE TRANSACTION-CHARGE = BUY-AMOUNT * 0.0000305.
               COMPUTE STT-CHARGE = BUY-AMOUNT * 0.000126.
               COMPUTE GST-CHARGE =
                   (BROKERAGE-CHARGE + TRANSACTION-CHARGE) * 0.18.
               COMPUTE TOTAL-CHARGE =
                BROKERAGE-CHARGE + TRANSACTION-CHARGE + STT-CHARGE +
                STAMP-DUTY + GST-CHARGE.
               COMPUTE MTM-AMOUNT = MTM-AMOUNT - TOTAL-CHARGE.
      *
       END PROGRAM CALC1002.
