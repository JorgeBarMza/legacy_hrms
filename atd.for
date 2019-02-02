      program atd
      integer u
      character employees*13, attendance*14, monthlyattendance*22
      character date_n*10, date_e*18
      parameter (u=20)

c get filenames as args
      CALL getarg(1, employees)
      CALL getarg(2, attendance)
      CALL getarg(3, monthlyattendance)

c open files
      open (10, FILE=employees)
      open (11, FILE=attendance)
      open (12, FILE=monthlyattendance)
      open (20, FILE='summaryfor.txt')
      open (21, FILE='monthly-attendancefor.txt')

c date conversion
      read(11, '(a)') date_n
c      write(*,*) date_n
c      write(*,*) date_n

c      write(*,*) date_e(date_n)
      call write_summary_header(date_e(date_n))

      end

c convert numeral date to english
      character*18 function date_e(date_n)
      character date_n*10
c date_n is formatted "yyy-mm-dd"
      character year*4, month*2, day*2, m_e*9
      character (len = 9), dimension (1 : 12) :: months
      integer m_i, day_i
      year = date_n(1:4)
      month = date_n(6:7)
      day = date_n(9:10)
      months (1:12) = (/ 'January  ', 'February ', 'March    ',
     & 'April    ', 'May      ', 'June     ', 'July     ', 'August   ',
     & 'September', 'October  ', 'November ', 'December '/)
      read(month,'(I2)') m_i
      m_e = months(m_i)
      if(day(1:1) .EQ. '0') then
        day = day(2:2)
      end if
c todo trim day
      date_e = m_e // ' ' // day // ', ' // year
      return
      end

      subroutine write_summary_header (date_e)
      character date_e*18, cols*58, dashes*58
      cols = 'Staff-ID Name' //
     &'                            ' //
     &'Department Status'
      dashes = '-------------------------------' //
     &'------------------------------'
      write(20,*) "Daily Attendance Summary"
      write(20,*) "Date: ", date_e
      write(20,*) cols
      write(20,*) dashes
      return
      end
