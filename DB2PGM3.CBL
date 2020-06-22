      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 20-06-2020
      * Purpose: INTERACTIVE COBOL DB2 PROGRAM
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. CBCUSINV.
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
           EXEC SQL INCLUDE CUSTOMER END-EXEC.
           EXEC SQL INCLUDE INVOICE  END-EXEC.
           EXEC SQL INCLUDE SQLCA   END-EXEC.
      *
       01 SWITCHES.
           05 END-OF-INQUIRIES-SW     PIC  X    VALUE 'N'.
              88 END-OF-INQUIRIES               VALUE 'Y'.
           05 CUSTOMER-FOUND-SW       PIC  X    VALUE 'Y'.
              88 CUSTOMER-FOUND                 VALUE 'Y'.
           05 VALID-CURSOR-SW         PIC  X    VALUE 'Y'.
              88 VALID-CURSOR                   VALUE 'Y'.
           05 END-OF-INVOICES-SW      PIC  X    VALUE 'N'.
              88 END-OF-INVOICES                VALUE 'Y'.
      *
      *INPUT PARAMETER
      *
       01 CUSTNO-WS                   PIC  X(6).
      *
       01 INV-TOTAL-FIELDS          COMP-3.
          05 INV-COUNT                PIC S9(5).
          05 INV-TOTAL                PIC S9(7)V9(2).
      *
       01 EDITED-TOTAL-FILEDS.
          05 EDITED-COUNT             PIC  Z(4)9.
          05 EDITED-TOTAL             PIC  Z(6)9.99.
      *
      *INVOICE CURSOR
      *
           EXEC SQL
              DECLARE INVCUR CURSOR WITH HOLD FOR
                 SELECT INVCUST,INVNO,INVDATE,INVTOTAL
                   FROM INV
                    WHERE INVCUST = :CUSTNO
           END-EXEC.
      *
       PROCEDURE DIVISION.
      *
       000-PROCESS-SALES-INQUIRES.
      *
            DISPLAY "***                                         ***".
            DISPLAY "CUSTOMER-SALES-INQUIRY-PLATFORM".
      *
            PERFORM 100-PROCESS-SALES-INQUIRY
               UNTIL END-OF-INQUIRIES.
      *
            DISPLAY "END OF SESSION, GOODBYE!".
            STOP RUN.
      *
       100-PROCESS-SALES-INQUIRY.
      *
            MOVE 'Y' TO CUSTOMER-FOUND-SW.
            PERFORM 110-ACCEPT-CUSTOMER-NUMBER.
            IF NOT END-OF-INQUIRIES
              PERFORM 120-GET-CUSTOMER-ROW
              PERFORM 130-DISPLAY-CUSTOMER-INFO
              IF CUSTOMER-FOUND
                 PERFORM 140-GET-INVOICES-INFO
                 PERFORM 200-DISPLAY-SALES-REPORT.
      *
       110-ACCEPT-CUSTOMER-NUMBER.
      *
            DISPLAY "***                                         ***".
            DISPLAY "TO END SESSION, ENTER 999999.".
            DISPLAY "ENTER THE CUSTOMER NO .".
            DISPLAY "-----------------------------------------------".
      *
            ACCEPT CUSTNO-WS.
      *
            MOVE CUSTNO-WS   TO CUSTNO.
      *
            IF CUSTNO-WS = 999999
                MOVE 'Y' TO END-OF-INQUIRIES-SW.
      *
       120-GET-CUSTOMER-ROW.
      *
           DISPLAY 'ENTERED CUSTOMER NUMBER IS: ' CUSTNO-WS.
      *
           EXEC SQL
              SELECT FNAME, LNAME
                INTO :FNAME, :LNAME
                FROM CUST
               WHERE CUSTNO = :CUSTNO
           END-EXEC.
      *
           IF SQLCODE NOT = 0
                 DISPLAY 'SQLCODE: ' SQLCODE
                 MOVE 'N' TO CUSTOMER-FOUND-SW.
      *
       130-DISPLAY-CUSTOMER-INFO.
      *
           IF CUSTOMER-FOUND
              DISPLAY 'CUSTOMER NO: ' CUSTNO-WS
              DISPLAY 'FIRST NAME : ' FNAME
              DISPLAY 'LAST NAME  : ' LNAME
            DISPLAY "-----------------------------------------------"
           ELSE
              DISPLAY 'YOU HAVE ENTERED INVALID CUSTOMER NO!'
              DISPLAY ' '
              DISPLAY ' CUSTOMER NUMBER' CUSTNO-WS ' NOT FOUND.'
              DISPLAY 'PLEASE TRY AGAIN LATER'.

      *
       140-GET-INVOICES-INFO.
      *
           MOVE 'Y' TO VALID-CURSOR-SW.
      *
           PERFORM 150-OPEN-INVOICE-CURSOR.
      *
           IF VALID-CURSOR
             MOVE 'N' TO END-OF-INVOICES-SW
             MOVE ZEROES   TO INV-COUNT
             MOVE ZEROES    TO INV-TOTAL
             PERFORM 160-GET-INVOICE-INFO
               UNTIL END-OF-INVOICES
             PERFORM 190-CLOSE-INVOICE-CURSOR.
      *
       150-OPEN-INVOICE-CURSOR.
      *
           EXEC SQL
              OPEN INVCUR
           END-EXEC.
      *
           IF SQLCODE NOT = 0
              MOVE 'N' TO VALID-CURSOR-SW
              DISPLAY 'SQLCODE: ' SQLCODE.

       160-GET-INVOICE-INFO.
      *
           PERFORM 170-FETCH-INVOICE-ROW.
      *
           IF NOT END-OF-INVOICES
             ADD 1        TO INV-COUNT
             ADD INVTOTAL TO INV-TOTAL
             PERFORM 180-DISPLAY-INVOICE-INFO.
      *
       170-FETCH-INVOICE-ROW.
      *
           EXEC SQL
              FETCH INVCUR
               INTO :INVCUST,
                    :INVNO,
                    :INVDATE,
                    :INVTOTAL
           END-EXEC.
      *
           IF SQLCODE NOT = 0
              MOVE 'Y' TO END-OF-INVOICES-SW
                IF SQLCODE NOT = 100
                   MOVE 'N'  TO VALID-CURSOR-SW
                   DISPLAY 'SQLCODE: ' SQLCODE.
      *
       180-DISPLAY-INVOICE-INFO.
      *
           MOVE INV-TOTAL    TO EDITED-TOTAL
           DISPLAY 'INVOICE NO: ' INVNO ' ' INVDATE ' ' EDITED-TOTAL.
      *
       190-CLOSE-INVOICE-CURSOR.
      *
           EXEC SQL
              CLOSE INVCUR
           END-EXEC.
      *
           IF SQLCODE NOT = 0
              MOVE 'N' TO VALID-CURSOR-SW
              DISPLAY 'SQLCODE: ' SQLCODE.
      *
       200-DISPLAY-SALES-REPORT.
      *
           IF VALID-CURSOR
              MOVE INV-TOTAL        TO EDITED-TOTAL
              MOVE INV-COUNT        TO EDITED-COUNT
              IF INV-TOTAL > 0
                 DISPLAY '-------                 -------'
              END-IF
                 DISPLAY ' TOTAL BILLED       :' EDITED-TOTAL
                 DISPLAY ' INVOICES  ISSUED   :' EDITED-COUNT
                 DISPLAY '-------                 -------'
           ELSE
             DISPLAY ' '
             DISPLAY ' *** INVOICE RETRIVAL ERROR *** '
             DISPLAY ' '.
      *
