      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 23-04-2020
      * Purpose: FORMAT COVID-19 DATA INTO REPORT FILE - 22-04-2020
      * Tectonics: COBC
      ******************************************************************
      *-----------------------*
       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. COVIDCBL.
       AUTHOR. SANDEEP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT STR-FILE ASSIGN TO INFILE
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
       FD  STR-FILE RECORDING MODE F.
       01  AMOUNT-REC.
           05 WS-STRING        PIC X(160).
      *
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05 WS-DATA          PIC X(208).
      *
       WORKING-STORAGE SECTION.
      *
       01  SYSTEM-DATE-AND-TIME.
           05  CURRENT-DATE.
               10  CURRENT-YEAR           PIC 9(02).
               10  CURRENT-MONTH          PIC 9(02).
               10  CURRENT-DAY            PIC 9(02).
           05  CURRENT-TIME.
               10  CURRENT-HOUR           PIC 9(02).
               10  CURRENT-MINUTE         PIC 9(02).
               10  CURRENT-SECOND         PIC 9(02).
               10  CURRENT-HNDSEC         PIC 9(02).
      *
       01  WS-FIELDS.
           05  WS-INFILE-STATUS           PIC X(02) VALUE SPACES.
           05  WS-OUTPUT-STATUS           PIC X(02) VALUE SPACES.
           05  WS-STR-FILE-OK             PIC X(01) VALUE 'N'.
           05  WS-STR-FILE-EOF            PIC X(01) VALUE 'N'.
           05  WS-COMMA                   PIC X(01) VALUE ','.
           05  WS-READ-RECORD             PIC 9(03) VALUE ZEROES.
           05  WS-WRITE-RECORD            PIC 9(03) VALUE ZEROES.
           05  ERR-MSG-DATA1              PIC X(35) VALUE SPACES.
           05  ERR-MSG-DATA2              PIC X(66) VALUE SPACES.
           05  WS-STRING1                 PIC X(45).
           05  WS-STRING2                 PIC X(45).
           05  WS-TEMP                    PIC X(45).
      *
       01  WS-NEW.
           05  WS-VALUE1                  PIC X(46).
           05  WS-VALUE2                  PIC X(11).
           05  WS-VALUE3                  PIC X(46).
           05  WS-VALUE4                  PIC 9(12).
           05  WS-TTL-VAL4                PIC 9(12).
           05  WS-VALUE5                  PIC 9(14).
           05  WS-TTL-VAL5                PIC 9(14).
           05  WS-VALUE6                  PIC 9(09).
           05  WS-TTL-VAL6                PIC 9(09).
           05  WS-VALUE7                  PIC 9(11).
           05  WS-TTL-VAL7                PIC 9(11).
           05  WS-VALUE8                  PIC 9(12).
           05  WS-TTL-VAL8                PIC 9(12).
           05  WS-VALUE9                  PIC 9(15).
           05  WS-TTL-VAL9                PIC 9(15).
           05  WS-VALUE10                 PIC X(22).
      *
      * COPYBOOK HEADER & BODY
       COPY COVIDHD.
       COPY COVIDBD.
      *
      *-----------------------*
       PROCEDURE DIVISION.
      *-----------------------*
       000-MAIN.
      *
           ACCEPT CURRENT-DATE FROM DATE.
           ACCEPT CURRENT-TIME FROM TIME.
      *
           DISPLAY "*************** COVID REPORT *********************".
           DISPLAY "STKCBL STARTED DATE = " CURRENT-MONTH "/"
                  CURRENT-DAY "/" CURRENT-YEAR  "  (mm/dd/yy)".
           DISPLAY "             TIME = " CURRENT-HOUR ":"
                  CURRENT-MINUTE ":" CURRENT-SECOND.
           DISPLAY "**************** 22-04-2020 **********************".
      *
           INITIALIZE WS-NEW.
      *
           PERFORM 100-OPEN-FILES.
           PERFORM 300-WRITE-OUTPUT-FILE-HEADER0.
           PERFORM 301-WRITE-OUTPUT-FILE-HEADER1.
      *
           PERFORM 200-PROCESS
              UNTIL WS-STR-FILE-EOF = 'Y'.
           PERFORM 400-CLOSE-FILES.
      *
           STOP RUN.
      *
         100-OPEN-FILES.
               OPEN INPUT  STR-FILE.
               OPEN OUTPUT OUT-FILE .
      *
         110-READ-INPUT-FILE.
               READ STR-FILE
                 AT END MOVE 'Y' TO WS-STR-FILE-EOF.
           EVALUATE WS-INFILE-STATUS
              WHEN '00'
              WHEN '04'
                  CONTINUE
              WHEN '10'
                  MOVE 'Y' TO WS-STR-FILE-EOF
                  INITIALIZE BODY
                  MOVE 'TTL VALUE'     TO VAL0
                  INSPECT VAL1 REPLACING ALL SPACES BY '-'
                  INSPECT VAL2 REPLACING ALL SPACES BY '-'
                  INSPECT VAL3 REPLACING ALL SPACES BY '-'
                  MOVE WS-TTL-VAL4 TO VAL4
                  MOVE WS-TTL-VAL5 TO VAL5
                  MOVE WS-TTL-VAL6 TO VAL6
                  MOVE WS-TTL-VAL7 TO VAL7
                  MOVE WS-TTL-VAL8 TO VAL8
                  MOVE WS-TTL-VAL9 TO VAL9
                  WRITE OUT-REC FROM BODY
                  ADD 1 TO WS-WRITE-RECORD
              WHEN OTHER
                  MOVE 'INPUT FILE I/O ERROR ON READ.  RC: '
                              TO ERR-MSG-DATA1
                  MOVE WS-INFILE-STATUS TO ERR-MSG-DATA2
                  DISPLAY ERR-MSG-DATA1
                  DISPLAY ERR-MSG-DATA2
           END-EVALUATE.
      *
         200-PROCESS.
           PERFORM 110-READ-INPUT-FILE.
              IF WS-STR-FILE-EOF NOT = 'Y' THEN
                 UNSTRING WS-STRING DELIMITED BY WS-COMMA
                    INTO
                    WS-VALUE1
                    WS-VALUE2
                    WS-VALUE3
                    WS-VALUE4
                    WS-VALUE5
                    WS-VALUE6
                    WS-VALUE7
                    WS-VALUE8
                    WS-VALUE9
                    WS-VALUE10
                 END-UNSTRING
      *
           DISPLAY "----------<< STRING DATA START >>--------"
           DISPLAY "-------------RECORD NUMBER" WS-READ-RECORD
           DISPLAY WS-VALUE1
           DISPLAY WS-VALUE2
           DISPLAY WS-VALUE3
           DISPLAY WS-VALUE4
           DISPLAY WS-VALUE5
           DISPLAY WS-VALUE6
           DISPLAY WS-VALUE7
           DISPLAY WS-VALUE8
           DISPLAY WS-VALUE9
           DISPLAY WS-VALUE10
           DISPLAY "----------<< STRING DATA END >>----------"
      *
                 ADD 1 TO WS-READ-RECORD
                 IF WS-WRITE-RECORD = 2 THEN
                    PERFORM 300-WRITE-OUTPUT-FILE-HEADER0 2 TIMES
                 END-IF
                 PERFORM 310-WRITE-OUTPUT-FILE-BODY
               END-IF.
      *
         300-WRITE-OUTPUT-FILE-HEADER0.
               IF WS-OUTPUT-STATUS = '00' THEN
                 WRITE OUT-REC FROM HEADER0
                 ADD 1 TO WS-WRITE-RECORD
               END-IF.
      *
          301-WRITE-OUTPUT-FILE-HEADER1.
               IF WS-OUTPUT-STATUS = '00' THEN
                 WRITE OUT-REC FROM HEADER1
                 ADD 1 TO WS-WRITE-RECORD
               END-IF.
      *
         310-WRITE-OUTPUT-FILE-BODY.
              IF WS-OUTPUT-STATUS = '00' THEN
                 MOVE WS-VALUE10(2:10)   TO VAL0
                 MOVE WS-VALUE1(2:)      TO WS-STRING1
                 INSPECT WS-STRING1 REPLACING ALL '"' BY SPACE
                 MOVE WS-STRING1         TO VAL1
                 MOVE WS-VALUE2(2:2)     TO VAL2
                 MOVE WS-VALUE3(2:)      TO WS-STRING1
                 INSPECT WS-STRING1 REPLACING ALL '"' BY SPACE
                 MOVE WS-STRING1         TO VAL3
                 MOVE WS-VALUE4          TO VAL4
                 COMPUTE WS-TTL-VAL4 = WS-TTL-VAL4 + WS-VALUE4
                 MOVE WS-VALUE5          TO VAL5
                 COMPUTE WS-TTL-VAL5 = WS-TTL-VAL5 + WS-VALUE5
                 MOVE WS-VALUE6          TO VAL6
                 COMPUTE WS-TTL-VAL6 = WS-TTL-VAL6 + WS-VALUE6
                 MOVE WS-VALUE7          TO VAL7
                 COMPUTE WS-TTL-VAL7 = WS-TTL-VAL7 + WS-VALUE7
                 MOVE WS-VALUE8          TO VAL8
                 COMPUTE WS-TTL-VAL8 = WS-TTL-VAL8 + WS-VALUE8
                 MOVE WS-VALUE9          TO VAL9
                 COMPUTE WS-TTL-VAL9 = WS-TTL-VAL9 + WS-VALUE9
                 WRITE OUT-REC FROM BODY
                 ADD 1 TO WS-WRITE-RECORD
               END-IF.
      *
                 EVALUATE WS-INFILE-STATUS
                    WHEN '00'
                       CONTINUE
                    WHEN OTHER
                       MOVE 'OUTPUT File Error ON WRITE.  RC: '
                                         TO ERR-MSG-DATA1
                       MOVE WS-INFILE-STATUS
                                         TO ERR-MSG-DATA2
                    DISPLAY ERR-MSG-DATA1
                    DISPLAY ERR-MSG-DATA2
                 END-EVALUATE.
      *
         400-CLOSE-FILES.
              CLOSE STR-FILE.
              CLOSE OUT-FILE.
      *
