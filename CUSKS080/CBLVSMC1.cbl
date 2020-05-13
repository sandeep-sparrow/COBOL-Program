      ******************************************************************
      * Author: SANDEEP R PRAJAPATI
      * Date: 30-04-2020
      * Purpose: COBOL PROGRAM READ VSAM FILE
      * Tectonics: CODC
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. CBLVSMC1.
       AUTHOR. SANDEEP P.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSKS080-FILE ASSIGN TO CUSKS080
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CUSKS080-KEY
               FILE STATUS IS CUSKS080-STATUS.
      *
           SELECT CUSKS081-FILE ASSIGN TO CUSKS081
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS CUSKS081-STATUS.
      ******************************************************************
       DATA DIVISION.
     `******************************************************************
       FILE SECTION.
       FD  CUSKS080-FILE.
       01  CUSKS080-RECORD.
           05 CUSKS080-KEY      PIC X(06).
           05 CUSKS080-DATA     PIC X(74).
      *
       FD  CUSKS081-FILE.
       01  CUSKS081-RECORD      PIC X(80).
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *    DATA STRUCTURE FOR TITLE                                    *
      ******************************************************************
       01  SAN-TITLE.
           05 T1   PIC X(11) VALUE '* CBLVSMC1 '.
           05 T2   PIC X(33) VALUE 'Sample, COBOL for VSAM I/O       '.
           05 T3   PIC X(24) VALUE 'https://www.google.com  '.
           05 FILLER
                   PIC X(12).
      *
       01  SAN-THANKS.
           05  C1 PIC X(11) VALUE '* CBLVSMC1 '.
           05  C2 PIC X(32) VALUE 'Thank you for using this program'.
           05  C3 PIC X(32) VALUE ' provided from SimoTime Technolo'.
           05  C4 PIC X(04) VALUE 'gies'.
      *
       01  CUSKS080-STATUS   PIC X(02) VALUE SPACES.
       01  CUSKS081-STATUS   PIC X(02) VALUE SPACES.
       01  WS-CUS-FILE.
           05  WS-CUS-FILE-OK         PIC X(01) VALUE 'N'.
           05  WS-CUS-FILE-EOF        PIC X(01) VALUE 'N'.
       01  WS-READ-RECORD             PIC 9(02) VALUE ZEROES.
       01  WS-WRITE-RECORD            PIC 9(02) VALUE ZEROES.
       01  WS-ERR-MSG                 PIC X(40).
       01  WS-ERR-CDE                 PIC X(02).
       01  WS-ERR-PROC                PIC X(20).
      *
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *
       A0001-MAIN.

           PERFORM B0001-OPEN-FILES   THRU B0001-EX
           PERFORM C0001-INIT-FILES   THRU C0001-EX

           PERFORM D0001-READ-FILES   THRU D0001-EX.
               IF WS-CUS-FILE-EOF NOT = 'Y' THEN
                   PERFORM E0001-PROCESS      THRU E0001-EX
                   UNTIL WS-CUS-FILE-EOF = 'Y'
               END-IF.

           DISPLAY 'TOTAL NUMBER OF RECORDS READS: ' WS-READ-RECORD.
           DISPLAY 'TOTAL NUMBER OF RECORDS WRITE: ' WS-WRITE-RECORD.

           PERFORM Z0001-CLOSE-FILES  THRU Z0001-EX.



       A0001-MAIN-EX.
           EXIT.
      *----------------------------------------------------------------*
       B0001-OPEN-FILES.
      *----------------------------------------------------------------*
               OPEN INPUT CUSKS080-FILE.

               IF CUSKS080-STATUS NOT EQUAL ZEROES
                   MOVE 'Error opening file CUSKS080'
                                             TO WS-ERR-MSG
                   MOVE CUSKS080-STATUS      TO WS-ERR-CDE
                   MOVE 'B0001-OPEN-FILES'   TO WS-ERR-PROC
                   PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
               END-IF.
      *
               OPEN INPUT CUSKS081-FILE.

               IF CUSKS081-STATUS NOT EQUAL ZEROES
                   MOVE 'Error opening file CUSKS081'
                                             TO WS-ERR-MSG
                   MOVE CUSKS081-STATUS      TO WS-ERR-CDE
                   MOVE 'B0001-OPEN-FILES'   TO WS-ERR-PROC
                   PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
               END-IF.

       B0001-EX.
           EXIT.
      *----------------------------------------------------------------*
       C0001-INIT-FILES.
      *----------------------------------------------------------------*
               WRITE CUSKS081-RECORD FROM SAN-TITLE.

               IF CUSKS081-STATUS NOT EQUAL ZEROES
                   MOVE 'Error writting file CUSKS081'
                                             TO WS-ERR-MSG
                   MOVE CUSKS081-STATUS      TO WS-ERR-CDE
                   MOVE 'C0001-INIT-FILES'   TO WS-ERR-PROC
                   PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
               ELSE
                   ADD 1 TO WS-WRITE-RECORD
               END-IF.
      *
              WRITE CUSKS081-RECORD FROM SAN-THANKS.

               IF CUSKS081-STATUS NOT EQUAL ZEROES
                   MOVE 'Error writting file CUSKS081'
                                             TO WS-ERR-MSG
                   MOVE CUSKS081-STATUS      TO WS-ERR-CDE
                   MOVE 'C0001-INIT-FILES'   TO WS-ERR-PROC
                   PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
               ELSE
                   ADD 1 TO WS-WRITE-RECORD
               END-IF.

       C0001-EX.
           EXIT.
      *----------------------------------------------------------------*
       D0001-READ-FILES.
      *----------------------------------------------------------------*
               READ CUSKS080-FILE.

                   EVALUATE CUSKS080-STATUS
                      WHEN '00'
                       ADD 1     TO WS-READ-RECORD
                      WHEN '10'
                       MOVE 'Y'  TO WS-CUS-FILE-EOF
                      WHEN OTHER
                       MOVE 'INPUT FILE I/O ERROR ON READ'
                                               TO WS-ERR-MSG
                       MOVE CUSKS080-STATUS    TO WS-ERR-CDE
                       MOVE 'D0001-READ-FILES' TO WS-ERR-PROC
                   END-EVALUATE.

       D0001-EX.
           EXIT.
      *----------------------------------------------------------------*
       E0001-PROCESS.
      *----------------------------------------------------------------*
                   MOVE CUSKS080-RECORD TO CUSKS081-RECORD
                   PERFORM F0001-WRITE-OUTPUT THRU F0001-EX
                   PERFORM D0001-READ-FILES   THRU D0001-EX.

       E0001-EX.
           EXIT.
      *----------------------------------------------------------------*
       F0001-WRITE-OUTPUT.
      *----------------------------------------------------------------*
               WRITE CUSKS081-RECORD.

               IF CUSKS081-STATUS NOT EQUAL ZEROES
                   MOVE 'Error writting file CUSKS081'
                                             TO WS-ERR-MSG
                   MOVE CUSKS081-STATUS      TO WS-ERR-CDE
                   MOVE 'F0001-WRITE-OUTPUT' TO WS-ERR-PROC
                   PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
               ELSE
                   ADD 1 TO WS-WRITE-RECORD
               END-IF.

       F0001-EX.
           EXIT.
      *----------------------------------------------------------------*
       Y0001-ERR-HANDLING.
      *----------------------------------------------------------------*
            DISPLAY '********************************'.
            DISPLAY '  ERROR HANDLING REPORT '.
            DISPLAY '********************************'.
            DISPLAY '  ' WS-ERR-MSG.
            DISPLAY '  ' WS-ERR-CDE.
            DISPLAY '  ' WS-ERR-PROC.
            DISPLAY '********************************'.

           PERFORM Z0001-CLOSE-FILES THRU Z0001-EX.

           Y0001-EXIT.
            EXIT.
      *----------------------------------------------------------------*
       Z0001-CLOSE-FILES.
      *----------------------------------------------------------------*
           CLOSE CUSKS080-FILE.

            IF CUSKS080-STATUS NOT EQUAL ZEROES
                MOVE 'Error CLOSING file CUSKS080'
                                          TO WS-ERR-MSG
                MOVE CUSKS080-STATUS      TO WS-ERR-CDE
                MOVE 'Z0001-CLOSE-FILES'  TO WS-ERR-PROC
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.
      *
            CLOSE CUSKS081-FILE.

            IF CUSKS081-STATUS NOT EQUAL ZEROES
               MOVE 'Error CLOSING file CUSKS081'
                                          TO WS-ERR-MSG
                MOVE CUSKS081-STATUS      TO WS-ERR-CDE
                MOVE 'Z0001-CLOSE-FILES'  TO WS-ERR-PROC
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.

            STOP RUN.

       Z0001-EX.
            EXIT.
