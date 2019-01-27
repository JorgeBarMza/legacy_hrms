           IDENTIFICATION DIVISION.
           PROGRAM-ID. FILES.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT MONTHLY-ATTENDANTS ASSIGN TO
                       'monthly-attendance.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT MONTHLY-ATTENDANTS-OUT ASSIGN TO
                       'monthly-attendancecob.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT EMPLOYEES ASSIGN TO
                       'employees.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL.

                SELECT ATTENDANTS ASSIGN TO
                      'attendance.txt'
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

           FD SUMMARIES.
           01 SUMMARY.
                 88 EOF-SUMMARY VALUE HIGH-VALUES.
                 02 SUMMARY-ID PIC 9(4).
                 02 SUMMARY-LAST-NAME PIC X(10).
                 02 SUMMARY-FIRST-NAME PIC X(20).
                 02 SUMMARY-DEPARTMENT PIC A(3).
                 02 SUMMARY-STATUS PIC A(6).

           WORKING-STORAGE SECTION.
           01 WS-MONTHLY-ATTENDANT.
             02 WS-ID PIC 9(4).
             02 WS-ABSENT PIC 9(3).
             02 WS-LATE PIC 9(3).
             02 WS-OVERTIME PIC 9(3).
           01 WS-EOF PIC A(1).

           PROCEDURE DIVISION.
           BEGIN.
             OPEN INPUT MONTHLY-ATTENDANTS.
             OPEN OUTPUT MONTHLY-ATTENDANTS-OUT.
                  READ MONTHLY-ATTENDANTS
                      AT END SET EOF-MONTHLY-ATTENDANT TO TRUE
                  END-READ
                  PERFORM UNTIL EOF-MONTHLY-ATTENDANT
                      WRITE MONTHLY-ATTENDANT-OUT FROM
                        MONTHLY-ATTENDANT
                      READ MONTHLY-ATTENDANTS
                          AT END SET EOF-MONTHLY-ATTENDANT TO TRUE
                      END-READ
                  END-PERFORM
                  DISPLAY "Finished writing file"
              CLOSE MONTHLY-ATTENDANTS.
              CLOSE MONTHLY-ATTENDANTS-OUT.
              STOP RUN.
