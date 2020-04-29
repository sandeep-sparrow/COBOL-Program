      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 21-04-2020
      * Purpose: DEDUCTION OF VARIOUS CHARGES ON INTRADAY AMOUNT
      * Tectonics: COBC
      ******************************************************************
      *-----------------------*
       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. STKCBL.
       AUTHOR. SANDEEP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
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
      *-----------------------*
       DATA DIVISION.
      *-----------------------*
       FILE SECTION.
      *
       FD  AMT-FILE RECORDING MODE F.
       01  AMOUNT-REC.
           05 TOTAL-MTM        PIC  999V99.
           05 TOTAL-BUY        PIC  9999V99.
           05 FILLER           PIC  X(69).
      *
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05  RPT-HEAD                   PIC  X(33).
           05  RPT-MM                     PIC  99.
           05  FILLER                     PIC  X     VALUE '/'.
           05  RPT-DD                     PIC  99.
           05  FILLER                     PIC  X     VALUE '/'.
           05  RPT-YY                     PIC  99.
           05  RPT-HEAD2                  PIC  X(20).
           05  RPT-HH                     PIC  99.
           05  FILLER                     PIC  X     VALUE ':'.
           05  RPT-MIN                    PIC  99.
           05  FILLER                     PIC  X     VALUE ':'.
           05  RPT-SS                     PIC  99.
           05  WS-SIGN                    PIC  X(01) VALUE '-'.
           05  FINAL-MTM                  PIC  999.999.
           05  FILLER                     PIC  X(03).
      *
       WORKING-STORAGE SECTION.
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
       01  WS-FIELDS.
           05  WS-INFILE-STATUS           PIC X(2)  VALUE SPACES.
           05  WS-OUTPUT-STATUS           PIC X(2)  VALUE SPACES.
           05  WS-REPORT-STATUS           PIC X(2)  VALUE SPACES.
      *
       01  WS-DEDUCTIONS.
           05 WS-BRKG-V                   PIC  9V99    VALUE .05.
           05 WS-T-CRG-V                  PIC  9V99999 VALUE .00305.
           05 WS-STT-V                    PIC  9V9999  VALUE 0.0126.
           05 WS-STAMP-D-V                PIC  9       VALUE 5.
           05 WS-GST-V                    PIC  9V99    VALUE .18.
      *
       01 WS-BRKG-V-E                     PIC  9.99.
       01 WS-T-CRG-V-E                    PIC  9.99999.
       01 WS-STT-V-E                      PIC  9.9999.
       01 WS-STAMP-D-V-E                  PIC  9.
       01 WS-GST-V-E                      PIC  9.99.
      *
       01  WS-BRKG                        PIC 999V9999.
       01  WS-T-CRG                       PIC 999V9999.
       01  WS-STT                         PIC 999V9999.
       01  WS-GST                         PIC 999V9999.
      *
       01  WS-BRKG-E                      PIC zzz9.9(04).
       01  WS-T-CRG-E                     PIC zzz9.9(04).
       01  WS-STT-E                       PIC zzz9.9(04).
       01  WS-GST-E                       PIC zzz9.9(04).
      *
       01  WS-SUM                         PIC 999V999.
       01  WS-SUM-E                       PIC 9(03).9(03).
      *
       01  WS-TOTAL-DEDUCTION             PIC 999V999.
       01  WS-TOTAL-DEDUCTION-E           PIC 9(03).9(03).
       01  WS-VALUE                       PIC 999V999.
       01  WS-VALUE-E                     PIC 999.999.
      *
      *-----------------------*
       PROCEDURE DIVISION.
      *-----------------------*
       000-MAIN.
      *
           ACCEPT CURRENT-DATE FROM DATE.
           ACCEPT CURRENT-TIME FROM TIME.
      *
           DISPLAY '************INTRADAY REPORT***********************'.
           DISPLAY 'STKCBL STARTED DATE = ' CURRENT-MONTH '/'
                  CURRENT-DAY '/' CURRENT-YEAR  '  (mm/dd/yy)'.
           DISPLAY '             TIME = ' CURRENT-HOUR ':'
                  CURRENT-MINUTE ':' CURRENT-SECOND.
           DISPLAY '************REPORT START**************************'.
      *
           INITIALIZE WS-TOTAL-DEDUCTION, WS-TOTAL-DEDUCTION-E,
                      WS-BRKG, WS-SUM-E, WS-GST-E, WS-VALUE,
                      WS-T-CRG, WS-BRKG-E,
                      WS-STT, WS-T-CRG-E,
                      WS-GST, WS-STT-E,
                      WS-SUM.
      *
           PERFORM 100-OPEN-FILES.
           PERFORM 800-INIT-REPORT.
           PERFORM 110-READ-INPUT-FILE.
      *
           DISPLAY "--------------------------".
           DISPLAY "TOTAL-MTM:" TOTAL-MTM.
           DISPLAY "TOTAL-BUY-ANOUNT:" TOTAL-BUY.
           DISPLAY "--------------------------".
      *
           PERFORM 200-COMPUTE-DEDUCTION.
           PERFORM 300-WRITE-OUTPUT-FILE.
           PERFORM 400-CLOSE-FILES.

           STOP RUN.
      *
         100-OPEN-FILES.
               OPEN INPUT  AMT-FILE.
               OPEN OUTPUT OUT-FILE .
      *
         110-READ-INPUT-FILE.
               READ AMT-FILE.
      *
         200-COMPUTE-DEDUCTION.
      *
               MOVE WS-BRKG-V TO WS-BRKG-V-E.
      *
               DISPLAY "--------------------------".
               DISPLAY "Overall Trade Deduction".
               DISPLAY "--------------------------".
      *
               DISPLAY "Brokerage value:" WS-BRKG-V-E.
               COMPUTE WS-BRKG = TOTAL-BUY * WS-BRKG-V.
               MOVE WS-BRKG TO WS-BRKG-E.
               DISPLAY "--------------------------".
               DISPLAY "Brokerage Amount:" WS-BRKG-E.
               DISPLAY "--------------------------".
      *
               MOVE WS-STT-V TO WS-STT-V-E.
      *
               DISPLAY "STT Charge Rate:" WS-STT-V-E.
               COMPUTE WS-STT = TOTAL-BUY * WS-STT-V.
               MOVE WS-STT TO WS-STT-E.
               DISPLAY "--------------------------".
               DISPLAY "STT Charge Amount:" WS-STT-E.
               DISPLAY "--------------------------".
      *
               MOVE WS-T-CRG-V TO WS-T-CRG-V-E.
      *
               DISPLAY "Transaction Charge Rate:" WS-T-CRG-V-E.
               COMPUTE WS-T-CRG = TOTAL-BUY * WS-T-CRG-V.
               MOVE WS-T-CRG TO WS-T-CRG-E.

               DISPLAY "Transaction Charge Amount:" WS-T-CRG-E.
               DISPLAY "--------------------------".
      *
               COMPUTE WS-SUM = WS-BRKG + WS-T-CRG.
               MOVE WS-SUM TO WS-SUM-E.
               DISPLAY "--------------------------".
               DISPLAY "WS-SUM-E:" WS-SUM-E.
               DISPLAY "--------------------------".
      *
               MOVE WS-GST-V TO WS-BRKG-V-E.
      *
               DISPLAY "GST Rate:" WS-GST-V-E.
               COMPUTE WS-GST = WS-SUM * WS-GST-V.
               MOVE WS-GST TO WS-GST-E.
               DISPLAY "--------------------------".
               DISPLAY "GST Amount:" WS-GST-E.
               DISPLAY "--------------------------".
      *
               COMPUTE WS-TOTAL-DEDUCTION  =
               WS-BRKG + WS-T-CRG + WS-STT + WS-STAMP-D-V + WS-GST.
               MOVE WS-TOTAL-DEDUCTION TO WS-TOTAL-DEDUCTION-E.
      *
               DISPLAY "--------------------------".
               DISPLAY "TOTAL-DEDUCTION:" WS-TOTAL-DEDUCTION-E.
               DISPLAY "--------------------------".
      *
               COMPUTE WS-VALUE = TOTAL-MTM - WS-TOTAL-DEDUCTION.
               MOVE WS-VALUE TO WS-VALUE-E.
      *
               DISPLAY "--------------------------".
               DISPLAY "Final Amount:" WS-VALUE-E.
               DISPLAY "--------------------------".

               IF TOTAL-MTM > WS-TOTAL-DEDUCTION
                 MOVE '+' TO WS-SIGN.


         300-WRITE-OUTPUT-FILE.
               IF WS-OUTPUT-STATUS = '00'
                   MOVE WS-VALUE-E TO FINAL-MTM.
                   WRITE OUT-REC.

         400-CLOSE-FILES.
              CLOSE AMT-FILE.
              CLOSE OUT-FILE.
      *
         800-INIT-REPORT.
               MOVE  CURRENT-YEAR                        TO RPT-YY.
               MOVE  CURRENT-MONTH                       TO RPT-MM.
               MOVE  CURRENT-DAY                         TO RPT-DD.
               MOVE  CURRENT-HOUR                        TO RPT-HH.
               MOVE  CURRENT-MINUTE                      TO RPT-MIN.
               MOVE  CURRENT-SECOND                      TO RPT-SS.
               MOVE  'CUSTOMER FILE UPDATE REPORT DATE:' TO RPT-HEAD.
               MOVE  ' (mm/dd/yy)   TIME: '              TO RPT-HEAD2.
      *
