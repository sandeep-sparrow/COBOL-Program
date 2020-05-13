      ******************************************************************
      * Author: SANDEEP PRAJAPATI
      * Date: 29-04-2020
      * Purpose: MULTI LEVEL TABLE
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MLTABEL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       01  STUDENT-TABLE.
           05 STUDENTS OCCURS 5 TIMES.
               10 STUDENT-NAME  PIC X(21).
               10 SUBJECT       PIC 9(03) OCCURS 3 TIMES.
      *
       01  WEEK-TABLE.
           05 DAY-NAME PIC X(10) OCCURS 7 TIMES.
       01  I           PIC 9(01).
       01  TO-DAY      PIC X(10) VALUE 'WEDNESDAY'.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello, world! - THIS IS A MUTLI LEVEL TABLE"
      *
            MOVE 'SANDEEP PRAJAPATI   |25 57 83 ' TO STUDENTS(1).
            MOVE 'BHARGAVI PRAJAPATI  |97 98 99 ' TO STUDENTS(2).
            MOVE 'NEHA SHARMA         |33 77 53 ' TO STUDENTS(3).
            MOVE 'SHIKHA GUPTA        |66 89 56 ' TO STUDENTS(4).
            MOVE 'PRIYA JAPE          |12 87 34 ' TO STUDENTS(5).
      *
            MOVE 'MONDAY'    TO DAY-NAME(1).
            MOVE 'TUESDAY'   TO DAY-NAME(2).
            MOVE 'WEDNESDAY' TO DAY-NAME(3).
            MOVE 'THURSDAY'  TO DAY-NAME(4).
            MOVE 'FRIDAY'    TO DAY-NAME(5).
            MOVE 'SATURDAY'  TO DAY-NAME(6).
            MOVE 'SUNDAY'    TO DAY-NAME(7).
      *
            DISPLAY '********************|*********'.
            DISPLAY STUDENTS(2).
            DISPLAY STUDENT-NAME(2).
            DISPLAY 'SUBJECT NO 2: 'SUBJECT(2 3).
            DISPLAY '********************|*********'.
            DISPLAY STUDENTS(5).
            DISPLAY STUDENT-NAME(5).
            DISPLAY 'SUBJECT NO 1: 'SUBJECT(5 1).
            DISPLAY '********************|*********'.
            DISPLAY 'STUDENT NAME         S1 S2 S3 '.
            DISPLAY '********************|*********'.
            DISPLAY STUDENTS(1).
            DISPLAY STUDENTS(2).
            DISPLAY STUDENTS(3).
            DISPLAY STUDENTS(4).
            DISPLAY STUDENTS(5).
            DISPLAY '********************|*********'.
      *
               DISPLAY'----------'
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
               DISPLAY DAY-NAME(I)
             END-PERFORM.
               DISPLAY'----------'
      *
               DISPLAY WEEK-TABLE.
      *
               DISPLAY'----------'
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
               IF DAY-NAME(I) = TO-DAY
                   DISPLAY 'TODAY IS - ' DAY-NAME(I) '- 29-04-2020'
               END-IF
             END-PERFORM.
               DISPLAY'----------'
      *
            STOP RUN.
       END PROGRAM MLTABEL.
