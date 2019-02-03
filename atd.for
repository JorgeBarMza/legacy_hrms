* CSCI3180 Principles of Programming Languages
*
* --- Declaration ---
*
* I declare that the assignment here submitted is original except
* for source material explicitly acknowledged. I also
* acknowledge that I am aware of University policy and
* regulations on honesty in academic work, and of the
* disciplinary guidelines and procedures applicable to breaches
* of such policy and regulations, as contained in the website
* http://www.cuhk.edu.hk/policy/academichonesty/
*
* Assignment 1
* Name : Jorge Alberto Barrios Mendoza
* Student ID : 1155128883
* Email Addr : 1155128883@link.cuhk.edu.hk
      program atd
      character employees*13, attendance*14, monthlyattendance*22
      character atts(10000)*100, empls(10000)*100
      character m_empls(10000)*100
      character date_n*10, date_e*18, att*100
      integer len_at, len_em, len_me dummy, a

c ******** main **********************************

c preprocess
      call read_args_and_open_files()
      read(11, '(a)') date_n
      call write_summary_header(date_e(date_n))
      call read_f(10, empls, len_em)
      call read_f(11, atts, len_at)
      call read_f(12, m_empls, len_me)
      call bubble(atts, len_at)
c process employees
      call prcs_e(empls,len_em, atts, len_at)

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
      len = 0
 10   read(u, '(A)', IOSTAT=ios) arr(id)
      if(ios.LT.0) return
      len = len+1
      id = id+1
      go to 10
      end

      subroutine prcs_e(empls,len_em, atts, len_at)
      character id*4, pad_1*5, lname*10, pad_2*1, fname*20
      character pad_3*1, dep*2, pad_4*8, stat*10, empl*100
      character smry*62, empls(10000)*100, atts(10000)*100
      integer emp_i, att_i, len_em, len_at
      emp_i = 1
      att_i = 1
 14   empl = empls(emp_i)
      if(emp_i.GT.len_em) return
      id = empl(1:4)
      pad_1 = "     "
      lname = empl(5:14)
      pad_2 = " "
      fname = empl(15:34)
      pad_3 = " "
      dep = empl(56:58)
      pad_4 = "        "
      call status(stat, empls, atts, emp_i, att_i)
      smry = id // pad_1 // lname // pad_2 // fname // pad_3 //
     & dep // pad_4 // stat
      write(20, '(A)') smry
      emp_i = emp_i + 1
      go to 14
      end

      subroutine status (stat, empls, atts, emp_i, att_i)
      character stat*10, empls(10000)*100, atts(10000)*100
      character att_id*4, emp_id*4, att_st*6
      integer emp_i, att_i, a_hour, a_min, l_hou, l_min
      integer late_p, over_p
      emp_id = empls(emp_i)(1:4)
      att_id = atts(att_i)(1:4)
      write (*,*) att_i
      if(emp_id.NE.att_id) go to 20
      att_st = atts(att_i)(5:10)
      if(att_st.EQ."LEAVE ") go to 24
c arrived
      read(atts(att_i)(22:23),'(i2)') a_hour
      read(atts(att_i)(25:26),'(i2)') a_min
c next attendant
      att_i = att_i + 1
      att_id = atts(att_i)(1:4)
      if(emp_id.NE.att_id) go to 21
      read(atts(att_i)(22:23),'(i2)') l_hour
      read(atts(att_i)(25:26),'(i2)') l_min
      write(*,*) "a_hour ", a_hour
      write(*,*) "a_min", a_min
      late_p = (a_hour*60 + a_min - 10*60) / 15
      over_p = l_hour-17
      att_i = att_i + 1
      write(*,*) "late " ,late_p
      if(late_p.GT.0) go to 22
      go to 23
  20  stat = "Absent"
      return
  21  stat = "Suspicious"
      return
  22  stat = "Late"
      return
  23  stat = "Present"
      return
  24  stat = "Suspicious"
      att_i = att_i + 1
      return
      end


      subroutine write_summary_header (date_e)
      character date_e*18, cols*58, dashes*58
      cols = 'Staff-ID Name' //
     &'                            ' //
     &'Department Status'
      dashes = '-------------------------------' //
     &'------------------------------'
      write(20,'(a)') 'Daily Attendance Summary'
      write(20,'(a)') "Date: " // date_e
      write(20,'(a)') cols
      write(20,'(a)') dashes
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
      character arr(10000)*100
      integer i,j,n
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
