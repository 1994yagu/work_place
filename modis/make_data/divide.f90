program delete

  implicit none
  integer ,parameter :: x=888
  integer ,parameter :: y=888
  integer :: i,j,k,n,irec
  integer*2 :: ref
  integer,dimension(1:2) :: d,h,m
  integer :: max,count1(500),count2(500)
  character :: file1
  character*64 :: filen1,filen2
  character :: hour1*2,min1*2,day1*3,day2*3,hour2*2,min2*2
  real :: time
  integer :: num

  open(10,file='rfile.txt',status='old')
  open(20,file='rfile1.txt',status='old')
  open(30,file='cul_file1.txt',status='replace')
  open(40,file='cul_file2.txt',status='replace')


  n=0
  read(10,'()')
  do
     read(10,*,end=100) filen1
     n=n+1
  enddo
100 continue

  rewind(10)
  read(10,'()')

! 2014151.0835.dat
  
  do k=1,n
     
     read(10,*) filen1
     read(20,*) filen2

!     print *,filen1
     day1 = filen1(5:7)
     hour1 = filen1(9:10)
     min1 = filen1(11:12)

!     print *,filen2
     day2 = filen2(5:7)
     hour2 = filen2(9:10)
     min2 = filen2(11:12)

     read(day1,*) d(1)
     read(day2,*) d(2)
     read(hour1,*) h(1)
     read(hour2,*) h(2)
     read(min1,*) m(1)
     read(min2,*) m(2)


     if(d(2).eq.d(1)) then
        time=(real(h(2))*60.+real(m(2)))*60.-(real(h(1))*60.+real(m(1)))*60.
     else
        time=(real(h(2))*60.+real(m(2)))*60.+((23.-real(h(1)))*60.+(60.-real(m(1))))*60.
     endif

     if((time.ge.3600).and.(time.le.10800)) then
        write(30,*) trim(adjustl(filen1))
        write(40,*) trim(adjustl(filen2))
     endif

 
  enddo




end program delete
