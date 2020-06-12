      ******************************************************************
      * Author: SANDEEP
      * Date: 10-06-2020
      * Purpose: TO UNDERSTAND ONE LEVEL TABLE AND SUBSCRIPTS
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTABLE.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT P-TABLE ASSIGN TO PTABLE
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS  IS  WS-FILEI-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  P-TABLE RECORDING MODE F.
       01  PRICE-TABLE-RECORD.
           05 PT-ITEM-NUMBER       PIC  9(03).
           05 FILLER               PIC  X(01).
           05 PT-ITEM-PRICE        PIC S99V99.
           05 FILLER               PIC  X(72).
      *
       WORKING-STORAGE SECTION.
      *
       01  SWITCHES.
           05 PTABLE-EOF-SWITCH    PIC X VALUE 'N'.
               88 PTABLE-EOF             VALUE 'Y'.
      *
       01  WS-FILEI-STATUS         PIC X(02) VALUE SPACES.
      *
       01  SUBSCRIPTS              BINARY.
           05 PRICE-TABLE-SUB      PIC S99.
           05  I                   PIC S99.
      *
       01  PRICE-TABLE             VALUE ZERO.
           05 PRICE-GROUP          OCCURS 16 TIMES.
               10 ITEM-NUMBER      PIC  9(03).
               10 ITEM-PRICE       PIC S99V99.
      *
       01 ITEM-PRICE-E      PIC ZZ9.99.
      *
       PROCEDURE DIVISION.
      *
            DISPLAY "HELLO WORLD!".
            DISPLAY "COBOL CODE TO LOAD DATA READ FROM FILE IN".
            DISPLAY "ONE LEVEL TABLE WITH THE USE OF SUBSCRIPTS".
      *
            INITIALIZE PRICE-TABLE.
      *
            PERFORM 000-OPEN-FILE.
      *
            PERFORM 100-READ-PRICE-TABLE.
            DISPLAY 'READ STATUS:' WS-FILEI-STATUS.
      *
              PERFORM 200-LOAD-PRICE-TABLE
                 VARYING PRICE-TABLE-SUB FROM 1 BY 1
                   UNTIL PTABLE-EOF OR
                         PRICE-TABLE-SUB > 16.
      *
            DISPLAY 'TABLE LOAD COMPLETED!'.
      *
            PERFORM 900-CLOSE-FILE.
      *
            DISPLAY 'CLOSE COMPLETED'.
      *
            DISPLAY '---------'.
            DISPLAY 'NO  PRICE'.
            DISPLAY '---------'.
      *
            PERFORM 300-DISPLAY-TABLE
               VARYING I FROM 1 BY 1 UNTIL I > 16.
      *
            DISPLAY '---------'.
      *
            STOP RUN.
      *
       000-OPEN-FILE.
               OPEN INPUT P-TABLE.
               DISPLAY 'OPEN STATUS:' WS-FILEI-STATUS.
      *
       100-READ-PRICE-TABLE.
               READ P-TABLE AT END SET PTABLE-EOF TO TRUE.
      *
       200-LOAD-PRICE-TABLE.
               MOVE PT-ITEM-NUMBER TO ITEM-NUMBER(PRICE-TABLE-SUB).
               MOVE PT-ITEM-PRICE  TO ITEM-PRICE(PRICE-TABLE-SUB).
               PERFORM 100-READ-PRICE-TABLE.
      *
       900-CLOSE-FILE.
               CLOSE P-TABLE.
               DISPLAY 'CLOSE STATUS:' WS-FILEI-STATUS.
      *
       300-DISPLAY-TABLE.
              MOVE ITEM-PRICE(I) TO ITEM-PRICE-E
              DISPLAY ITEM-NUMBER(I), ITEM-PRICE-E.
      *
       END PROGRAM CTABLE.
