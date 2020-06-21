      ******************************************************************
      * Author: SANDEEP R PRAJAPATI
      * Date: 22-04-2020
      * Purpose: FUNCTION OF COBOL
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNCTIONCBL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NUM       PIC 999V99.
       01 WS-NUM1      PIC 999.99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            DISPLAY FUNCTION MAX(13, 14, 15).
            DISPLAY FUNCTION MIN(13, 14, 15).
            DISPLAY FUNCTION SUM(13, 14, 15).
            DISPLAY FUNCTION MEAN(13, 14, 15).
            DISPLAY FUNCTION REVERSE(13).
            DISPLAY FUNCTION REVERSE('SANDEEP').
            DISPLAY FUNCTION LENGTH('SANDEEP').

            DISPLAY FUNCTION SUM(13.13, 13.13).
            COMPUTE WS-NUM ROUNDED = FUNCTION SUM(13.13, 13.13).
            COMPUTE WS-NUM1 =  FUNCTION SQRT(144).
            DISPLAY WS-NUM.
            DISPLAY WS-NUM1.
            DISPLAY FUNCTION REM(12, 5).
            DISPLAY FUNCTION MOD(5.0, 2.5)
      * MOD(NO DECIMAL),REM(NO DECIMAL) FUNCTION HAS 9 BYTES
      * PRE-DEFINED.
            DISPLAY FUNCTION FACTORIAL(5).
            STOP RUN.
       END PROGRAM FUNCTIONCBL.
