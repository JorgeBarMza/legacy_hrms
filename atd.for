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
      year = date_n(1:4)
      month = date_n(6:7)
      day = date_n(9:10)
      if(month.EQ.'01') m_e = 'January'
      if(month.EQ.'02') m_e = 'Februay'
      if(month.EQ.'03') m_e = 'March'
      if(month.EQ.'04') m_e = 'April'
      if(month.EQ.'05') m_e = 'May'
      if(month.EQ.'06') m_e = 'June'
      if(month.EQ.'07') m_e = 'July'
      if(month.EQ.'08') m_e = 'August'
      if(month.EQ.'09') m_e = 'September'
      if(month.EQ.'10') m_e = 'October'
      if(month.EQ.'11') m_e = 'November'
      if(month.EQ.'12') m_e = 'December'
      if(day(1:1) .EQ. '0') day = day(2:2)
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
