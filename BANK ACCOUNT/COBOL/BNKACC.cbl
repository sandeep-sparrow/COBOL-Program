      *--------------------------
       IDENTIFICATION DIVISION.
      *--------------------------
       PROGRAM-ID.  BNKACC.
      *--------------------------
       ENVIRONMENT DIVISION.
      *--------------------------
       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT BALANCE-AMT ASSIGN TO BALFILE
                  ORGANIZATION IS SEQUENTIAL.
           SELECT DEPOSIT-AMT ASSIGN TO DEPFILE
                  ORGANIZATION IS SEQUENTIAL.
           SELECT WITHDRW-AMT ASSIGN TO WITFILE
                  ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-AMT  ASSIGN TO OUTFILE
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
      *--------------------------
       DATA DIVISION.
      *--------------------------
       FILE SECTION.
       FD BALANCE-AMT
            RECORD CONTAINS 80 CHARACTERS
            RECORDING MODE IS F.
       01 BALANCE-IO.
           02  BAL-AMT         PIC  9999999.99.
           02  FILLER          PIC  X(70).
      *
       FD DEPOSIT-AMT
            RECORD CONTAINS 80 CHARACTERS
            RECORDING MODE IS F.
       01 DEPOSIT-I.
          02  DEP-AMT         PIC  9999999.99.
          02  FILLER          PIC  X(70).
      *
       FD WITHDRW-AMT
            RECORD CONTAINS 80 CHARACTERS
            RECORDING MODE IS F.
       01 WITHDRW-I.
          02  DRAW-AMT        PIC  9999999.99.
          02  FILLER          PIC  X(70).
      *
       FD OUTPUT-AMT
            RECORD CONTAINS 80 CHARACTERS
            RECORDING MODE IS F.
       01 OUTPUT-O.
          02  FIN-AMT         PIC $9,99,9999.99.
          02  FILLER          PIC  X(67).
      *
       WORKING-STORAGE SECTION.
      *
        01 WS-NEW              PIC  9(10).
        01 WS-BAL              PIC  9(10).
        01 WS-DEP              PIC  9(10).
        01 WS-DRAW             PIC  9(10).
        01 WS-BAL-AMT          PIC  9999999.99.
        01 WS-FIN-AMT          PIC  9999999.99.
        77 WS-STATUS           PIC  X(02).
      *--------------------------
       PROCEDURE DIVISION.
      *--------------------------
       MAIN-PARA.
      *
           INITIALIZE WS-NEW,
                      WS-BAL,
                      WS-DEP,
                      WS-DRAW,
                      WS-BAL-AMT,
                      WS-FIN-AMT,
      *
           PERFORM OPEN-FILES.
           PERFORM READ-FILES.
           PERFORM WRITE-RECORD.
           PERFORM CLOSE-FILES.
            STOP RUN.
      *
       OPEN-FILES.
           OPEN INPUT  BALANCE-AMT.
           OPEN INPUT  DEPOSIT-AMT.
           OPEN INPUT  WITHDRW-AMT.
           OPEN OUTPUT OUTPUT-AMT.
              DISPLAY 'OUTPUT FILE STATUS: ' WS-STATUS.
      *
       READ-FILES.
           READ BALANCE-AMT.
           READ DEPOSIT-AMT.
           READ WITHDRW-AMT.
      *
              DISPLAY 'BANK BALANCE IS     : ' BAL-AMT.
              DISPLAY 'DEPOSITED AMOUNT IS : ' DEP-AMT.
              DISPLAY 'WITHDRAW AMOUNT IS  : ' DRAW-AMT.
      *
             MOVE BAL-AMT     TO WS-BAL.
             MOVE DEP-AMT     TO WS-DEP.
             MOVE DRAW-AMT    TO WS-DRAW.
      *
              DISPLAY 'WS-BAL  IS : ' WS-BAL.
              DISPLAY 'WS-DEP  IS : ' WS-DEP.
              DISPLAY 'WS-DRAW IS : ' WS-DRAW.
      *
           IF WS-DEP IS NOT = ZEROES THEN
             COMPUTE WS-NEW = WS-BAL + WS-DEP
             MOVE WS-NEW      TO WS-BAL-AMT
             MOVE WS-BAL-AMT  TO WS-BAL
              DISPLAY 'AMOUNT DEPOSITED SUCCESSFULL!'
            END-IF.
      *
              DISPLAY 'ACCOUNT BALANCE AFTER DEPOSIT IS: ' WS-BAL-AMT.
      *
           IF WS-DRAW IS NOT = ZEROES THEN
             COMPUTE WS-NEW  = WS-BAL - WS-DRAW
             MOVE WS-NEW      TO WS-BAL-AMT
              DISPLAY 'AMOUNT WITHDRAW SUCCESSFULL!'
            END-IF.
      *
              DISPLAY 'ACCOUNT BALANCE AFTER WITHDRW IS: ' WS-BAL-AMT.
              DISPLAY 'WS-STATUS : ' WS-STATUS.
      *
       WRITE-RECORD.
           IF WS-STATUS IS = ZEROES THEN
              DISPLAY 'WS-BAL-AMT: ' WS-BAL-AMT
            MOVE WS-BAL-AMT    TO  WS-FIN-AMT
              DISPLAY 'WS-FIN-AMT: ' WS-FIN-AMT
            MOVE WS-FIN-AMT    TO  FIN-AMT
              DISPLAY 'FIN-AMT: ' FIN-AMT
            WRITE OUTPUT-O
           END-IF.
      *
       CLOSE-FILES.
           CLOSE BALANCE-AMT.
           CLOSE DEPOSIT-AMT.
           CLOSE WITHDRW-AMT.
           CLOSE OUTPUT-AMT.
      *
