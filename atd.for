      program atd
      integer u
      character(4) s
      character(13) employees
      character(14) attendance
      character(22) monthlyattendance
      parameter (u=20)

      CALL getarg(1, employees)
      CALL getarg(2, attendance)
      CALL getarg(3, monthlyattendance)

      open (10, FILE=employees)
      open (10, FILE=attendance)
      open (10, FILE=monthlyattendance)
      open (20, FILE='summaryfor.txt')

      read(10, '(a)') s
c      write(11,*) s
      write(20,*) employees
      write(20,*) attendance
      write(20,*) monthlyattendance
      end
