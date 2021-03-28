       ID DIVISION.
       PROGRAM-ID.   GETENVX.
       AUTHOR.       Sandeep R Prajapati.
       DATE-WRITTEN.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE ASSIGN TO DD1.
       DATA DIVISION.
       FILE SECTION.
       FD  TEST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  TEST-REC           PIC X(80).
           EJECT
       WORKING-STORAGE SECTION.

       01  OUTPUT-REC.
           05  REC-NUM         PIC 9(10).
           05  FILLER          PIC X(70)        VALUE SPACES.

       01 WS-WORK-DSN PIC X(55) VALUE 'TTT.TTT.TTT'.
       01 WS-DYNAMIC-OPEN-AREA.
           05 FILE-NAME.
               10 FILLER PIC X(8)  VALUE 'DD1=DSN('.
               10 DSNAME PIC X(55) VALUE SPACE.
               10 FILLER PIC X(50) VALUE
                   ' NEW TRACKS SPACE(100,100) UNIT(SYSDA) CATALOG '.
               10 FILLER PIC X(01) VALUE X'00'.
           05 FILE-PTR POINTER.
           05 RC PIC S9(9) BINARY VALUE ZERO.
       01  FNAMES.
           05  FNAME-01             PIC X(55)    VALUE
               'Z80076.PS.TESTFILE.OUT01'.
           05  FNAME-02             PIC X(55)    VALUE
               'Z80076.PS.TESTFILE.OUT02'.
           05  FNAME-03             PIC X(55)    VALUE
               'Z80076.PS.TESTFILE.OUT03'.
           05  FNAME-04             PIC X(55)    VALUE
               'Z80076.PS.TESTFILE.OUT04'.
       01  FNAME-TABLE REDEFINES FNAMES.
           05  FNAME                PIC X(55)    OCCURS 4 TIMES.
       01  SUB                      PIC S9(9)    COMP-3.
       LINKAGE SECTION.
           EJECT
       PROCEDURE DIVISION.

           PERFORM
           VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE SPACE TO DSNAME
               MOVE FNAME(SUB)              TO WS-WORK-DSN
               STRING WS-WORK-DSN DELIMITED BY SPACE
                   ')' DELIMITED BY SIZE
                   INTO DSNAME
               DISPLAY 'FILE-NAME = ' FILE-NAME
               SET FILE-PTR TO ADDRESS OF FILE-NAME
               CALL 'PUTENV' USING BY VALUE FILE-PTR RETURNING RC
               IF RC NOT = 0
               THEN
                   MOVE RC     TO RETURN-CODE
                   GOBACK
               END-IF
               OPEN OUTPUT TEST-FILE
               PERFORM
               10 TIMES
                   ADD 1 TO REC-NUM
                   WRITE TEST-REC FROM OUTPUT-REC
               END-PERFORM
               CLOSE TEST-FILE
           END-PERFORM.

           MOVE 0           TO RETURN-CODE.

           GOBACK.
