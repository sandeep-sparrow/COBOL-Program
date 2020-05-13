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
       FILE-CONTROL.
      *
           SELECT AMT-FILE ASSIGN TO INFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-INFILE-STATUS.
      *
           SELECT OUT-FILE ASSIGN TO OUTFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-OUTPUT-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  AMT-FILE RECORDING MODE F.
       01  AMOUNT-REC.
           05 MTM-AMOUNT       PIC  9(07)V99.
           05 FILLER           PIC  X   VALUE SPACE.
           05 BUY-AMOUNT       PIC  9(08)V99.
           05 FILLER           PIC  X(60).
      *
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05  RPT-HEAD1                  PIC  X(29).
           05  WS-SIGN                    PIC  X(01).
           05  FINAL-MTM                  PIC  Z,ZZZ,ZZZ.99.
           05  FILLER                     PIC  X.
           05  RPT-MM                     PIC  99.
           05  WS-F1                      PIC  X.
           05  RPT-DD                     PIC  99.
           05  WS-F2                      PIC  X.
           05  RPT-YY                     PIC  99.
           05  RPT-HEAD2                  PIC  X(20).
           05  RPT-HH                     PIC  99.
           05  WS-F3                      PIC  X.
           05  RPT-MIN                    PIC  99.
           05  WS-F4                      PIC  X.
           05  RPT-SS                     PIC  99.
           05  FILLER                     PIC  X(01).

       WORKING-STORAGE SECTION.
      *
       01  WS-FIELDS.
           05  WS-INFILE-STATUS           PIC X(2)  VALUE SPACES.
           05  WS-OUTPUT-STATUS           PIC X(2)  VALUE SPACES.
      *
       01  SYSTEM-DATE-AND-TIME.
           05  CURRENT-DATE.
               10  CURRENT-YEAR           PIC 9(2).
               10  CURRENT-MONTH          PIC 9(2).
               10  CURRENT-DAY            PIC 9(2).
           05  CURRENT-TIME.
               10  CURRENT-HOUR           PIC 9(2).
               10  CURRENT-MINUTE         PIC 9(2).
               10  CURRENT-SECOND         PIC 9(2).
               10  CURRENT-HNDSEC         PIC 9(2).
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
           05 MTM-AMOUNT-E            PIC Z,ZZZ,ZZZ.99.
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
      *
           ACCEPT CURRENT-DATE FROM DATE.
           ACCEPT CURRENT-TIME FROM TIME.
      *
           PERFORM 100-OPEN-FILES.
           PERFORM 800-INIT-REPORT.
           PERFORM 110-READ-INPUT-FILE.
           PERFORM 200-CALCULATE-NXT.
      *
               MOVE TOTAL-CHARGE TO TOTAL-CHARGE-EDITED
               MOVE MTM-AMOUNT   TO MTM-AMOUNT-EDITED
               MOVE BUY-AMOUNT   TO BUY-AMOUNT-EDITED
      *
           DISPLAY '************INTRADAY REPORT***********************'.
           DISPLAY 'CALC1002 STARTED DATE = ' CURRENT-MONTH '/'
                  CURRENT-DAY '/' CURRENT-YEAR  '  (mm/dd/yy)'.
           DISPLAY '             TIME = ' CURRENT-HOUR ':'
                  CURRENT-MINUTE ':' CURRENT-SECOND.
           DISPLAY '************REPORT START**************************'.
           DISPLAY "TOTAL BUY AMOUNT           = " BUY-AMOUNT-EDITED.
           DISPLAY "MTM AMOUNT                 = " MTM-AMOUNT-E.
           DISPLAY "TOTAL CHARGES              = " TOTAL-CHARGE-EDITED.
           DISPLAY "MTM AMOUNT AFTER DEDUCTION = " MTM-AMOUNT-EDITED.
           DISPLAY '************REPORT END****************************'.
      *
           PERFORM 300-WRITE-OUTPUT-FILE.
           PERFORM 400-CLOSE-FILES.
      *
            DISPLAY "END OF SESSION".
      *
            STOP RUN.
      *
         100-OPEN-FILES.
               OPEN INPUT  AMT-FILE.
               OPEN OUTPUT OUT-FILE .
      *
         110-READ-INPUT-FILE.
               READ AMT-FILE.
      *
       200-CALCULATE-NXT.
      *
               COMPUTE BROKERAGE-CHARGE = BUY-AMOUNT * 0.0005.
               COMPUTE TRANSACTION-CHARGE = BUY-AMOUNT * 0.0000305.
               COMPUTE STT-CHARGE = BUY-AMOUNT * 0.000126.
               COMPUTE GST-CHARGE =
                   (BROKERAGE-CHARGE + TRANSACTION-CHARGE) * 0.18.
               COMPUTE TOTAL-CHARGE =
                BROKERAGE-CHARGE + TRANSACTION-CHARGE + STT-CHARGE +
                STAMP-DUTY + GST-CHARGE.
                    MOVE MTM-AMOUNT TO MTM-AMOUNT-E.
               COMPUTE MTM-AMOUNT = MTM-AMOUNT - TOTAL-CHARGE.
      *
         300-WRITE-OUTPUT-FILE.
               IF WS-OUTPUT-STATUS = '00'
                   MOVE MTM-AMOUNT TO FINAL-MTM.
                   WRITE OUT-REC.

         400-CLOSE-FILES.
              CLOSE AMT-FILE.
              CLOSE OUT-FILE.
      *
         800-INIT-REPORT.
               MOVE '/'                                  TO WS-F1.
               MOVE '/'                                  TO WS-F2.
               MOVE ':'                                  TO WS-F3.
               MOVE ':'                                  TO WS-F4.
               MOVE  CURRENT-YEAR                        TO RPT-YY.
               MOVE  CURRENT-MONTH                       TO RPT-MM.
               MOVE  CURRENT-DAY                         TO RPT-DD.
               MOVE  CURRENT-HOUR                        TO RPT-HH.
               MOVE  CURRENT-MINUTE                      TO RPT-MIN.
               MOVE  CURRENT-SECOND                      TO RPT-SS.
               MOVE  'CUST FILE UPDATE REPORT DATE:'     TO RPT-HEAD1.
               MOVE  ' (mm/dd/yy)   TIME: '              TO RPT-HEAD2.

       END PROGRAM CALC1002.
