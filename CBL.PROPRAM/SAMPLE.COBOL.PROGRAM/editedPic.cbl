      ******************************************************************
      * Author: sandeep prajapati
      * Date: 04-05-202
      * Purpose: Edited Picture
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  X PIC 999999 VALUE 000056.
       01  Z PIC 9 VALUE 4.
       01  NUM PIC 99.
       01  X-E PIC 999,999.
       01  X-EE PIC 9999V99.
       01  Y PIC 9999.99 VALUE '1234.66'.
       01  Y-E PIC 9999V99.
       01  ACC-LIMIT PIC S9(9)V99 VALUE ZERO.
       01  WS-LIMIT  PIC $$$,$$$,$$$9.99.
       01  EMPLOYEE-TABLE.
           05 EMPLOYEE-DETAILS OCCURS 5 TIMES.
               10 EMP-NAME    PIC X(10).
               10 EMP-SALARY  PIC 9(05).
       01  SALARY PIC 9(05) OCCURS 5 TIMES.
       01  I PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY'THERE ARE 4 TYPES OF INSERTION EDITING'
            DISPLAY'SIMPLE INSERTION'
            MOVE X TO X-E.
            MOVE X TO X-EE.
            DISPLAY X.
            DISPLAY X-E.
            COMPUTE NUM = X-EE / Z.
            DISPLAY NUM.
            DISPLAY X-EE.
            DISPLAY'SPECIAL INSERTION'
            MOVE Y TO Y-E.
            DISPLAY "Hello world!"
            DISPLAY Y.
            DISPLAY Y-E.
            MOVE 123456789.11 TO ACC-LIMIT.
            DISPLAY ACC-LIMIT.
            MOVE ACC-LIMIT TO WS-LIMIT.
            DISPLAY WS-LIMIT.

            MOVE 'SANDEEP   37500' TO EMPLOYEE-DETAILS(1).
            MOVE 'HEENAL    23500' TO EMPLOYEE-DETAILS(2).
            MOVE 'RAMESH    17500' TO EMPLOYEE-DETAILS(3).
            MOVE 'GAYATARI  87500' TO EMPLOYEE-DETAILS(4).
            MOVE 'UGANDA    77500' TO EMPLOYEE-DETAILS(5).


            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF EMP-SALARY(I) > 35000
                   DISPLAY EMPLOYEE-DETAILS(I)
               END-IF
            END-PERFORM.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
