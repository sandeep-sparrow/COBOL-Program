      ******************************************************************
      * Author: sandeep
      * Date: 25-04-2020
      * Purpose: statistical function
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SATSFUNCTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            DISPLAY "MARKS OF 10 STUDENTS".
            DISPLAY FUNCTION MEAN(15 10 05 03 07 08 16 20 12 16).
            DISPLAY FUNCTION MEDIAN(15 10 05 03 07 08 16 20 12 16).
            DISPLAY FUNCTION STANDARD-DEVIATION(15 10 05 03 07 08 16
            20 12 16).
            DISPLAY FUNCTION VARIANCE(15 10 05 03 07 08 16 20 12 16).
            DISPLAY FUNCTION RANGE(15 10 05 03 07 08 16 20 12 16).
            DISPLAY FUNCTION MIDRANGE(15 10 05 03 07 08 16 20 12 16).
            STOP RUN.
       END PROGRAM SATSFUNCTION.
