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
                 02 MONTHLY-ATTENDANT-OVERTIME PIC 9(3).

           FD MONTHLY-ATTENDANTS-OUT.
           01 MONTHLY-ATTENDANT-OUT.
                 88 EOF-MONTHLY-ATTENDANT-OUT VALUE HIGH-VALUES.
                 02 MONTHLY-ATTENDANT-OUT-ID PIC 9(4).
                 02 MONTHLY-ATTENDANT-OUT-ABSENT PIC 9(3).
                 02 MONTHLY-ATTENDANT-OUT-LATE PIC 9(3).
                 02 MONTHLY-ATTENDANT-OUT-OVERTIME PIC 9(3).

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
      *           YYY-MM-DD-HH:NN
                 02 ATTENDANT-DATETIME.
                    03 ATTENDANT-YEAR PIC 9(4).
                    03 ATTENDANT-DASH1 PIC X.
                    03 ATTENDANT-MONTH PIC 9(2).
                    03 ATTENDANT-DASH2 PIC X.
                    03 ATTENDANT-DAY PIC 9(2).
                    03 ATTENDANT-DASH3 PIC X.
                    03 ATTENDANT-HOUR PIC 9(2).
                    03 ATTENDANT-COLON PIC X.
                    03 ATTENDANT-MINUTE PIC 9(2).

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
                 02 SUMMARY-ID PIC 9(4).
                 02 SUMMARY-PADDING1 PIC X(5).
                 02 SUMMARY-LAST-NAME PIC X(10).
                 02 SUMMARY-PADDING2 PIC X(1).
                 02 SUMMARY-FIRST-NAME PIC X(20).
                 02 SUMMARY-PADDING3 PIC X(1).
                 02 SUMMARY-DEPARTMENT PIC A(3).
                 02 SUMMARY-PADDING4 PIC X(8).
                 02 SUMMARY-STATUS PIC A(10).

           WORKING-STORAGE SECTION.
           01 WS-MONTHLY-ATTENDANT.
             02 WS-MONTHLY-ATTENDANT-ID PIC 9(4).
             02 WS-MONTHLY-ATTENDANT-ABSENT PIC 9(3).
             02 WS-MONTHLY-ATTENDANT-LATE PIC 9(3).
             02 WS-MONTHLY-ATTENDANT-OVERTIME PIC 9(3).
           01 WS-EMPLOYEES-FILE-STATUS.
             05 WS-EMPLOYEES-STATUS-KEY-1 PIC X.
           01 WS-ATTENDANTS-SORTED-FILE-STATUS.
             05 WS-ATTENDANTS-SORTED-STATUS-KEY-1 PIC X.
           01 WS-MONTHLY-ATTENDANTS-FILE-STATUS.
             05 WS-MONTHLY-ATTENDANTS-STATUS-KEY-1 PIC X.
           01 WS-SUMMARY.
             02 WS-SUMMARY-ID PIC 9(4).
             02 WS-SUMMARY-PADDING1 PIC X(5) VALUE "     ".
             02 WS-SUMMARY-LAST-NAME PIC X(10).
             02 WS-SUMMARY-PADDING2 PIC X(1) VALUE " ".
             02 WS-SUMMARY-FIRST-NAME PIC X(20).
             02 WS-SUMMARY-PADDING3 PIC X(1) VALUE " ".
             02 WS-SUMMARY-DEPARTMENT PIC A(3).
             02 WS-SUMMARY-PADDING4 PIC X(8) VALUE "        ".
             02 WS-SUMMARY-STATUS PIC A(10).
      * EXTRA
             01 WS-ATTENDANT-ARRIVED PIC 9 VALUE 0.
             01 WS-ATTENDANT-DATETIME-ARRIVE.
                02 WS-ATTENDANT-DATETIME-ARRIVE-YEAR PIC 9(4).
                02 WS-ATTENDANT-DATETIME-ARRIVE-DASH1 PIC X.
                02 WS-ATTENDANT-DATETIME-ARRIVE-MONTH PIC 9(2).
                02 WS-ATTENDANT-DATETIME-ARRIVE-DASH2 PIC X.
                02 WS-ATTENDANT-DATETIME-ARRIVE-DAY PIC 9(2).
                02 WS-ATTENDANT-DATETIME-ARRIVE-DASH3 PIC X.
                02 WS-ATTENDANT-DATETIME-ARRIVE-HOUR PIC 9(2).
                02 WS-ATTENDANT-DATETIME-ARRIVE-COLON PIC X.
                02 WS-ATTENDANT-DATETIME-ARRIVE-MINUTE PIC 9(2).
             01 WS-ATTENDANT-DATETIME-LEAVE.
                02 WS-ATTENDANT-DATETIME-LEAVE-YEAR PIC 9(4).
                02 WS-ATTENDANT-DATETIME-LEAVE-DASH1 PIC X.
                02 WS-ATTENDANT-DATETIME-LEAVE-MONTH PIC 9(2).
                02 WS-ATTENDANT-DATETIME-LEAVE-DASH2 PIC X.
                02 WS-ATTENDANT-DATETIME-LEAVE-DAY PIC 9(2).
                02 WS-ATTENDANT-DATETIME-LEAVE-DASH3 PIC X.
                02 WS-ATTENDANT-DATETIME-LEAVE-HOUR PIC 9(2).
                02 WS-ATTENDANT-DATETIME-LEAVE-COLON PIC X.
                02 WS-ATTENDANT-DATETIME-LEAVE-MINUTE PIC 9(2).
             01 WS-LATE-PERIODS PIC 9(3).
             01 WS-OVERTIME-HOURS PIC 9(3).
             01 WS-SHOULD-READ-ATTENDANT PIC 9 VALUE 1.
             01 WS-TITLE PIC X(24) VALUE
               "Daily Attendance Summary".
             01 WS-COLUMNS.
                02 F PIC X(13) VALUE "Staff-ID Name".
                02 F PIC X(28) VALUE "                            ".
                02 F PIC X(17) VALUE "Department Status".
             01 WS-DASHES.
                02 F PIC X(31) VALUE '-------------------------------'.
                02 F PIC X(31) VALUE '-------------------------------'.
             01 WS-PRESENCE.
                02 F PIC X(21) VALUE "Number of Presences: ".
                02 WS-PRESENCES-VALUE-DISPLAY PIC zzzz.
             01 WS-ABSENCE.
                02 F PIC X(20) VALUE "Number of Absences: ".
                02 WS-ABSENCES-VALUE-DISPLAY PIC zzzz.
             01 WS-LATE-ARRIVAL.
                02 F PIC X(25) VALUE "Number of Late Arrivals: ".
                02 WS-LATE-VALUE-DISPLAY PIC zzzz.
             01 WS-SUSPICIOUS.
                02 F PIC X(30) VALUE "Number of Suspicious Records: ".
                02 WS-SUSPICIOUS-VALUE-DISPLAY PIC zzzz.
             01 WS-PRESENCES-VALUE PIC 9(4).
             01 WS-ABSENCES-VALUE PIC 9(4).
             01 WS-LATE-VALUE PIC 9(4).
             01 WS-SUSPICIOUS-VALUE PIC 9(4).
             01 WS-SUMMARY-DATE.
                02 WS-SUMMARY-DATE-YEAR PIC 9999.
                02 DASH1 PIC X.
                02 WS-SUMMARY-DATE-MONTH PIC 99.
                02 DASH2 PIC X.
                02 WS-SUMMARY-DATE-DAY PIC 99.
             01 WS-SUMMARY-DATE-ENGLISH.
                02 F PIC X(6) VALUE "Date: ".
                02 WS-SUMMARY-DATE-ENGLISH-MONTH PIC X(9).
                02 SPACE1 PIC X VALUE " ".
                02 WS-SUMMARY-DATE-ENGLISH-DAY PIC 9(2).
                02 SPACE2 PIC XX VALUE ", ".
                02 WS-SUMMARY-DATE-ENGLISH-YEAR PIC 9999.
             01 WS-ABSENT PIC 999.
             01 WS-LATE PIC 999.
             01 WS-OVERTIME PIC 999.
             01 WS-MONTHLY-DATE.
                02 WS-MONTHLY-DATE-YEAR PIC 9999.
                02 DASH PIC X VALUE "-".
                02 WS-MONTHLY-DATE-MONTH PIC 99.
             01 WS-FIRST-DAY-OF-MONTH PIC 9.

           PROCEDURE DIVISION.
           BEGIN.
             OPEN INPUT MONTHLY-ATTENDANTS
             OPEN INPUT ATTENDANTS
             OPEN INPUT EMPLOYEES
             OPEN OUTPUT MONTHLY-ATTENDANTS-OUT
             OPEN OUTPUT SUMMARIES
             SORT ATTENDANTS-WORK ON ASCENDING KEY
                 ATTENDANT-SORTED-ID USING ATTENDANTS GIVING
                 ATTENDANTS-SORTED
             OPEN INPUT ATTENDANTS-SORTED.

           WRITE-SUMMARY-HEADER.
             WRITE SUMMARY FROM WS-TITLE
             PERFORM PROCESS-HEADER-DATES
             WRITE SUMMARY FROM WS-SUMMARY-DATE-ENGLISH
             WRITE SUMMARY FROM WS-COLUMNS
             WRITE SUMMARY FROM WS-DASHES.

           PROCESS-EMPLOYEES.
      * EXPERIMENT
      * END EXPERIMENT
              READ EMPLOYEES
              IF WS-EMPLOYEES-STATUS-KEY-1 = "1"
                GO TO WRITE-SUMMARY-FOOTER
              END-IF
              PERFORM PROCESS-EMPLOYEE
              GO TO PROCESS-EMPLOYEES.

           PROCESS-EMPLOYEE.
              PERFORM FILLUP-SUMMARY
              IF WS-SHOULD-READ-ATTENDANT = 0 OR
                 WS-ATTENDANTS-SORTED-STATUS-KEY-1 NOT = "1"
                IF WS-SHOULD-READ-ATTENDANT = 1
                  READ ATTENDANTS-SORTED
                END-IF
                IF EMPLOYEE-ID EQUALS ATTENDANT-SORTED-ID
                  PERFORM PROCESS-ATTENDANT
                END-IF
                IF EMPLOYEE-ID NOT EQUALS ATTENDANT-SORTED-ID
                  MOVE 0 TO WS-SHOULD-READ-ATTENDANT
                END-IF
              END-IF
              WRITE SUMMARY FROM WS-SUMMARY
              PERFORM UPDATE-MONTHLY-ATTENDANT.

           WRITE-SUMMARY-FOOTER.
             MOVE WS-PRESENCES-VALUE TO WS-PRESENCES-VALUE-DISPLAY
             MOVE WS-ABSENCES-VALUE TO WS-ABSENCES-VALUE-DISPLAY
             MOVE WS-LATE-VALUE TO WS-LATE-VALUE-DISPLAY
             MOVE WS-SUSPICIOUS-VALUE TO WS-SUSPICIOUS-VALUE-DISPLAY
             WRITE SUMMARY FROM WS-DASHES
             WRITE SUMMARY FROM WS-PRESENCE
             WRITE SUMMARY FROM WS-ABSENCE
             WRITE SUMMARY FROM WS-LATE-ARRIVAL
             WRITE SUMMARY FROM WS-SUSPICIOUS
             GO TO FINISH.

      * HELPER FUNCTIONS

           UPDATE-MONTHLY-ATTENDANT.
             READ MONTHLY-ATTENDANTS
             MOVE MONTHLY-ATTENDANT-ID TO WS-MONTHLY-ATTENDANT-ID
             IF WS-FIRST-DAY-OF-MONTH EQUALS 1
               MOVE 0 TO WS-MONTHLY-ATTENDANT-ABSENT
               MOVE 0 TO WS-MONTHLY-ATTENDANT-LATE
               MOVE 0 TO WS-MONTHLY-ATTENDANT-OVERTIME
             END-IF
             IF NOT WS-FIRST-DAY-OF-MONTH EQUALS 1
               MOVE MONTHLY-ATTENDANT-ABSENT TO
                 WS-MONTHLY-ATTENDANT-ABSENT
               MOVE MONTHLY-ATTENDANT-LATE TO WS-MONTHLY-ATTENDANT-LATE
               MOVE MONTHLY-ATTENDANT-OVERTIME TO
                 WS-MONTHLY-ATTENDANT-OVERTIME
             END-IF
             ADD WS-ABSENT TO WS-MONTHLY-ATTENDANT-ABSENT
             ADD WS-LATE TO WS-MONTHLY-ATTENDANT-LATE
             ADD WS-OVERTIME TO WS-MONTHLY-ATTENDANT-OVERTIME
             WRITE MONTHLY-ATTENDANT-OUT FROM WS-MONTHLY-ATTENDANT.

           PROCESS-ATTENDANT.
             IF ATTENDANT-SORTED-STATUS NOT = "ARRIVE"
               MOVE "SUSPICIOUS" TO WS-SUMMARY-STATUS
               ADD 1 TO WS-SUSPICIOUS-VALUE
               SUBTRACT 1 FROM WS-ABSENCES-VALUE
               MOVE 0 TO WS-ABSENT
               MOVE 1 TO WS-SHOULD-READ-ATTENDANT
             END-IF
             IF ATTENDANT-SORTED-STATUS = "ARRIVE"
                 MOVE ATTENDANT-SORTED-DATETIME TO
                   WS-ATTENDANT-DATETIME-ARRIVE
                 IF WS-ATTENDANTS-SORTED-STATUS-KEY-1 NOT = "1"
                   READ ATTENDANTS-SORTED
                 END-IF
                 IF EMPLOYEE-ID EQUALS ATTENDANT-SORTED-ID
                   MOVE ATTENDANT-SORTED-DATETIME TO
                     WS-ATTENDANT-DATETIME-LEAVE
                   COMPUTE WS-LATE-PERIODS =
                     (WS-ATTENDANT-DATETIME-ARRIVE-HOUR * 60 +
                      WS-ATTENDANT-DATETIME-ARRIVE-MINUTE -
                      10 * 60) / 15
                   IF WS-LATE-PERIODS > 0
                     MOVE "LATE" TO WS-SUMMARY-STATUS
                     ADD 1 TO WS-LATE-VALUE
                     MOVE 1 TO WS-LATE
                     SUBTRACT 1 FROM WS-ABSENCES-VALUE
                     MOVE 0 TO WS-ABSENT
                   END-IF
                   IF WS-LATE-PERIODS = 0
                     MOVE "PRESENT" TO WS-SUMMARY-STATUS
                     ADD 1 TO WS-PRESENCES-VALUE
                     SUBTRACT 1 FROM WS-ABSENCES-VALUE
                     MOVE 0 TO WS-ABSENT
                     MOVE 1 TO WS-SHOULD-READ-ATTENDANT
                   END-IF
                   COMPUTE WS-OVERTIME =
                     WS-ATTENDANT-DATETIME-LEAVE-HOUR - 17
                   ADD WS-OVERTIME TO WS-OVERTIME-HOURS
                 END-IF
               END-IF.

           FILLUP-SUMMARY.
              MOVE EMPLOYEE-ID TO WS-SUMMARY-ID
              MOVE EMPLOYEE-LAST-NAME TO WS-SUMMARY-LAST-NAME
              MOVE EMPLOYEE-FIRST-NAME TO WS-SUMMARY-FIRST-NAME
              MOVE EMPLOYEE-DEPARTMENT TO WS-SUMMARY-DEPARTMENT
              MOVE "ABSENT" TO WS-SUMMARY-STATUS
              ADD 1 TO WS-ABSENCES-VALUE
              MOVE 1 TO WS-ABSENT
              MOVE 0 TO WS-LATE
              MOVE 0 TO WS-OVERTIME
              MOVE 0 TO WS-MONTHLY-ATTENDANT-ABSENT
              MOVE 0 TO WS-MONTHLY-ATTENDANT-LATE
              MOVE 0 TO WS-MONTHLY-ATTENDANT-OVERTIME.

           PROCESS-HEADER-DATES.
              OPEN INPUT ATTENDANTS
              READ ATTENDANTS
              MOVE ATTENDANT TO WS-SUMMARY-DATE
              MOVE WS-SUMMARY-DATE-DAY TO WS-SUMMARY-DATE-ENGLISH-DAY
              MOVE WS-SUMMARY-DATE-YEAR TO WS-SUMMARY-DATE-ENGLISH-YEAR
              IF WS-SUMMARY-DATE-MONTH EQUALS 01
                 MOVE "January" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 02
                 MOVE "February" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 03
                 MOVE "March" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 04
                 MOVE "April" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 05
                 MOVE "May" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 06
                 MOVE "June" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 07
                 MOVE "July" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 08
                 MOVE "August" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 09
                 MOVE "September" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 10
                 MOVE "October" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 11
                 MOVE "November" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
              IF WS-SUMMARY-DATE-MONTH EQUALS 12
                 MOVE "December" TO WS-SUMMARY-DATE-ENGLISH-MONTH
              END-IF
      * CHECK FIRST DAY OF MONTH
              READ MONTHLY-ATTENDANTS
              MOVE WS-SUMMARY-DATE-YEAR TO WS-MONTHLY-DATE-YEAR
              MOVE WS-SUMMARY-DATE-MONTH TO WS-MONTHLY-DATE-MONTH
              IF WS-SUMMARY-DATE-DAY EQUALS 01
                MOVE 1 TO WS-FIRST-DAY-OF-MONTH
              END-IF
              IF WS-SUMMARY-DATE-DAY NOT EQUALS 01
                MOVE 0 TO WS-FIRST-DAY-OF-MONTH
              END-IF
              WRITE MONTHLY-ATTENDANT-OUT FROM WS-MONTHLY-DATE.

          FINISH.
              DISPLAY "Finished writing file".
              CLOSE ATTENDANTS-SORTED, MONTHLY-ATTENDANTS, EMPLOYEES,
                    MONTHLY-ATTENDANTS-OUT, SUMMARIES, ATTENDANTS.
          STOP RUN.
