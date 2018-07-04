program RMT
  implicit none
  integer :: num,i
  real :: cu,cv,ca,mu,mv,ma,ic
  real :: ru,rv,sru,srv
  character*64 :: f1,f2

  open(10,file='all_compare.txt',status='old')

  num=0
  do
     read(10,*,end=50)
     num=num+1
  enddo

50 continue

  rewind(10)

  ru=0
  rv=0
  sru=0
  srv=0
  
  do i=1,num
     read(10,*) cu,mu,cv,mv,f1,f2
     ru=ru+(cu-mu)**2
     rv=rv+(cv-mv)**2
     sru=cu+sru
     srv=cv+srv
  enddo

  ru=sqrt(ru/real(num))
  rv=sqrt(rv/real(num))
  sru=sru/real(num)
  srv=srv/real(num)

  print *,ru,rv
  print *,sru,srv
end program RMT

     
