           IDENTIFICATION DIVISION.
           PROGRAM-ID. FILES.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT MONTHLY-ATTENDANTS ASSIGN TO
                       'monthly-attendance.txt'
                       FILE STATUS IS WS-MONTHLY-ATTENDANTS-FILE-STATUS
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT MONTHLY-ATTENDANTS-OUT ASSIGN TO
                       'monthly-attendancecob.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT EMPLOYEES ASSIGN TO
                       'employees.txt'
                       FILE STATUS IS WS-EMPLOYEES-FILE-STATUS
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT ATTENDANTS ASSIGN TO
                      'attendance.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT ATTENDANTS-WORK ASSIGN TO
                      'attendance-work.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      ACCESS MODE IS SEQUENTIAL.

                SELECT ATTENDANTS-SORTED ASSIGN TO
                      'attendance-sorted.txt'
                       FILE STATUS IS WS-ATTENDANTS-SORTED-FILE-STATUS
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT SUMMARIES ASSIGN TO
                      'summarycob.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      ACCESS MODE IS SEQUENTIAL.

           DATA DIVISION.
           FILE SECTION.
           FD MONTHLY-ATTENDANTS.
           01 MONTHLY-ATTENDANT.
                 88 EOF-MONTHLY-ATTENDANT VALUE HIGH-VALUES.
                 02 MONTHLY-ATTENDANT-ID PIC 9(4).
                 02 MONTHLY-ATTENDANT-ABSENT PIC 9(3).
                 02 MONTHLY-ATTENDANT-LATE PIC 9(3).
                 02 MONTHLY-ATTENDANT-SUSPICIOUS PIC 9(3).

           FD MONTHLY-ATTENDANTS-OUT.
           01 MONTHLY-ATTENDANT-OUT.
                 88 EOF-MONTHLY-ATTENDANT-OUT VALUE HIGH-VALUES.
                 02 MONTHLY-ATTENDANT-OUT-ID PIC 9(4).
                 02 MONTHLY-ATTENDANT-OUT-ABSENT PIC 9(3).
                 02 MONTHLY-ATTENDANT-OUT-LATE PIC 9(3).
                 02 MONTHLY-ATTENDANT-OUT-SUSPICIOUS PIC 9(3).

           FD EMPLOYEES.
           01 EMPLOYEE.
                 88 EOF-EMPLOYEE VALUE HIGH-VALUES.
                 02 EMPLOYEE-ID PIC 9(4).
                 02 EMPLOYEE-LAST-NAME PIC X(10).
                 02 EMPLOYEE-FIRST-NAME PIC X(20).
                 02 EMPLOYEE-GENDER PIC A(1).
                 02 EMPLOYEE-DATE-OF-BIRTH PIC X(10).
                 02 EMPLOYEE-DATE-OF-HIRING PIC X(10).
                 02 EMPLOYEE-DEPARTMENT PIC A(3).
                 02 EMPLOYEE-MONTHLY-SALARY PIC 9(6).

           FD ATTENDANTS.
           01 ATTENDANT.
                 88 EOF-ATTENDANT VALUE HIGH-VALUES.
                 02 ATTENDANT-ID PIC 9(4).
                 02 ATTENDANT-STATUS PIC A(6).
                 02 ATTENDANT-DATETIME PIC X(16).

           SD ATTENDANTS-WORK.
           01 ATTENDANT-WORK.
                 88 EOF-ATTENDANT VALUE HIGH-VALUES.
                 02 ATTENDANT-WORK-ID PIC 9(4).
                 02 ATTENDANT-WORK-STATUS PIC A(6).
                 02 ATTENDANT-WORK-DATETIME PIC X(16).

           FD ATTENDANTS-SORTED.
           01 ATTENDANT-SORTED.
                 88 EOF-ATTENDANT VALUE HIGH-VALUES.
                 02 ATTENDANT-SORTED-ID PIC 9(4).
                 02 ATTENDANT-SORTED-STATUS PIC A(6).
                 02 ATTENDANT-SORTED-DATETIME PIC X(16).

           FD SUMMARIES.
           01 SUMMARY.
                 88 EOF-SUMMARY VALUE HIGH-VALUES.
                 02 SUMMARY-ID PIC 9(4).
                 02 SUMMARY-LAST-NAME PIC X(10).
                 02 SUMMARY-FIRST-NAME PIC X(20).
                 02 SUMMARY-DEPARTMENT PIC A(3).
                 02 SUMMARY-STATUS PIC A(6).

      *     WORKING-STORAGE SECTION.
      *     01 WS-MONTHLY-ATTENDANT.
      *       02 WS-ID PIC 9(4).
      *       02 WS-ABSENT PIC 9(3).
      *       02 WS-LATE PIC 9(3).
      *       02 WS-OVERTIME PIC 9(3).
      *     01 WS-EOF PIC A(1).
           01 WS-EMPLOYEES-FILE-STATUS.
            05 WS-EMPLOYEES-STATUS-KEY-1 PIC X.
           01 WS-ATTENDANTS-SORTED-FILE-STATUS.
            05 WS-ATTENDANTS-SORTED-STATUS-KEY-1 PIC X.
           01 WS-MONTHLY-ATTENDANTS-FILE-STATUS.
            05 WS-MONTHLY-ATTENDANTS-STATUS-KEY-1 PIC X.

           PROCEDURE DIVISION.
           BEGIN.
             OPEN INPUT MONTHLY-ATTENDANTS.
             OPEN INPUT ATTENDANTS.
             OPEN INPUT EMPLOYEES.
             OPEN OUTPUT MONTHLY-ATTENDANTS-OUT.
             OPEN OUTPUT SUMMARIES.

             SORT ATTENDANTS-WORK ON ASCENDING KEY
                 ATTENDANT-SORTED-ID USING ATTENDANTS GIVING
                 ATTENDANTS-SORTED.
             OPEN INPUT ATTENDANTS-SORTED.

      *     WRITE-SUMMARY-HEADER.

           PROCESS-EMPLOYEE.
              READ EMPLOYEES.
              READ ATTENDANTS-SORTED.
              IF WS-EMPLOYEES-STATUS-KEY-1 = "1" OR
                  WS-ATTENDANTS-SORTED = "1"
                GO TO PROCESS-ATTENDANT
              

           PROCESS-ATTENDANT.
              READ MONTHLY-ATTENDANTS
              IF WS-MONTHLY-ATTENDANTS-STATUS-KEY-1 = "1"
                GO TO FINISH
              WRITE MONTHLY-ATTENDANT-OUT FROM MONTHLY-ATTENDANT.
              GO TO PROCESS-ATTENDANT.

          FINISH.
              DISPLAY "Finished writing file".
              CLOSE ATTENDANTS-SORTED
              CLOSE MONTHLY-ATTENDANTS.
              CLOSE EMPLOYEES
              CLOSE MONTHLY-ATTENDANTS-OUT.
              CLOSE SUMMARIES.
          STOP RUN.
