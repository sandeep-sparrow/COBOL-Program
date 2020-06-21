      ******************************************************************
      * Author: sandeep
      * Date: 25-04-2020
      * Purpose: mathematical functions
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATHFUNCTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            DISPLAY FUNCTION SUM(5,15).
            DISPLAY FUNCTION SQRT(9).
            DISPLAY FUNCTION REM(13,3).
            DISPLAY FUNCTION MOD(13,3).
            DISPLAY FUNCTION FACTORIAL(5).
            DISPLAY FUNCTION INTEGER(-4.12).
            DISPLAY FUNCTION INTEGER-PART(-4.12).
            DISPLAY FUNCTION NUMVAL("     123").
            DISPLAY FUNCTION NUMVAL-C("$   123").

            STOP RUN.
       END PROGRAM MATHFUNCTION.
