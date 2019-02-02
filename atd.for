      program atd
      character employees*13, attendance*14, monthlyattendance*22
      character atts(10000)*100, empl(10000)*100
      character m_empl(10000)*100
      character date_n*10, date_e*18, att*100
      integer len_at, dummy, a

c ******** main **********************************

c preprocess
      call read_args_and_open_files()
      read(11, '(a)') date_n
      call write_summary_header(date_e(date_n))
      call read_f(10, empl, len_at)
      call read_f(11, atts, dummy)
      call read_f(12, m_empl, dummy)
      call bubble(atts, len_at)
      end

c ******** helper functions **********************

c convert numeral date to english
      character*18 function date_e(date_n)
      character date_n*10
c date_n is formatted "yyyy-mm-dd"
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

      subroutine read_f (u,arr, len)
      character arr(10000)*100
      integer id, u, len
      id = 1
      len = 1
 10   read(u, '(A)', IOSTAT=ios) arr(id)
      if(ios.LT.0) return
      len = len+1
      id = id+1
      go to 10
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

      subroutine read_args_and_open_files ()
      character employees*13, attendance*14, monthlyattendance*22
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
      return
      end

      subroutine swap(arr,a,b)
      character arr(10000)*100, temp*100
      integer a,b
      temp=arr(a)
      arr(a)=arr(b)
      arr(b)=temp
      return
      end

      subroutine bubble(arr, n)
      character arr(10000)*100, id_1, id_2
      integer i,j,n, a
      i = 0
 12   i = i+1
      if(i.GT.n) go to 11
        j = n+1
 13     j = j-1
        if(j.LT.(i+1)) go to 12
          if(arr(j-1).GT.arr(j)) call swap(arr,j-1,j)
        go to 13
 11   return
      end
