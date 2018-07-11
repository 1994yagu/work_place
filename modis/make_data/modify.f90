program make

implicit none
integer, parameter :: x=888
integer, parameter :: y=888
integer :: i,j,k,n,irec
integer*2 :: ref(x,y),ref1,ref2
integer :: max
character :: file1
character*64 :: filen,mfile
character(64),dimension(1500) :: name
character*2 :: hour,min
integer :: h(1500),m(1500)

 open(10,file='file.txt',status='old')
! open(15,file='rfile3.txt',status='replace')
 open(40,file='file1.txt',status='old')
! open(45,file='rfile2.txt',status='replace')

!goto 1000

 n=0
 do
 	read(10,*,end=100) filen
 	n=n+1
 enddo
100 continue

print *,n

 rewind(10)
 do k=1,n
 	read(10,*) filen
	name(k) = filen
! 	print *,filen
	hour = filen(19:20)
	min = filen(21:22)
	read(hour,*) h(k)
	read(min,*) m(k)
!	print *,h(k),m(k)
 enddo
 
 
do k=1,n
	open(20,file=name(k),form='unformatted',access='direct',recl=2,status='old')
!	if(name(k).eq.'MOD02QKM.A2016169.0550.dat') then
	open(30,file='r'//name(k),form='unformatted',access='direct',recl=2,status='replace')
	print *,name(k)
	irec=0
	do j=1,y
	 do i=1,x
	  irec=irec+1
	  read(20,rec=irec) ref(i,j)
	 enddo
	enddo
	
	irec=0
	do j=1,y
	 do i=1,x
	   irec=irec+1
	   write(30,rec=irec) ref(i,y+1-j)
	 enddo
	enddo
	close(30)
!	endif
	close(20)
enddo

print *,'aqua'

 n=0
 do
 	read(40,*,end=200) filen
 	n=n+1
 enddo
200 continue

print *,n

 rewind(40)
 do k=1,n
 	read(40,*) filen
	name(k) = filen
! 	print *,filen
	hour = filen(19:20)
	min = filen(21:22)
	read(hour,*) h(k)
	read(min,*) m(k)
!	print *,h(k),m(k)
 enddo
 
 
do k=1,n
	open(50,file=name(k),form='unformatted',access='direct',recl=2,status='old')
!	if(name(k).eq.'MOD02QKM.A2016169.0410.dat') then
	open(60,file='r'//name(k),form='unformatted',access='direct',recl=2,status='replace')
	print *,name(k)
	irec=0
	do j=1,y
	 do i=1,x
	  irec=irec+1
	  read(50,rec=irec) ref(i,j)
	 enddo
	enddo
	
	irec=0
	do j=1,y
	 do i=1,x
	   irec=irec+1
	   write(60,rec=irec) ref(i,y+1-j)
	 enddo
	enddo
	close(60)
!	endif
	close(50)
enddo
 
 end program
