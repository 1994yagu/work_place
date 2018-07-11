program delete

implicit none
integer ,parameter :: x=888
integer ,parameter :: y=888
integer :: i,j,k,n,irec
integer*2 :: ref
integer :: max,count1(1500),count2(1500),d(1500),dd(1500),h(1500)
character :: file1
character*64 :: filen
character :: hour*2,min*2,day1*3,day2*3
integer :: num

 open(10,file='file.txt',status='old')
 open(30,file='file1.txt',status='old')


 n=0
 read(10,'()')
 do
 	read(10,*,end=100) filen
 	n=n+1
 enddo
100 continue

 rewind(10)
 read(10,'()')
 
  do k=1,n
 	d(k)=0
 	read(10,*) filen
 	print *,filen
	day1 = filen(15:17)
	hour = filen(19:20)
	min = filen(21:22)
	read(day1,*) d(k)
 enddo
 
  rewind(10)
 read(10,'()')
 
 do k=1,n
 	d(k)=0
 	read(10,*) filen
 	print *,filen
	day1 = filen(15:17)
	hour = filen(19:20)
	min = filen(21:22)
	read(day1,*) d(k)
	read(hour,*) h(k)
	!print *,num
	 
	open(20,file=filen,status='old',form='unformatted',access='direct',recl=2)
	irec=0
	count1(k)=0
	max=0
	do j=1,y
	 do i=1,x
	  irec=irec+1
	  read(20,rec=irec) ref
	  if(ref.ge.max) max=ref
	  if(ref.eq.-1) count1(k)=count1(k)+1
	!  print *,max
	 enddo
	enddo
	if(count1(k).ge.x*y*0.1) then
	 print *,count1(k)
	 print *,d(k),d(k-1),d(k+1)
	 if((d(k).eq.d(k-1)).or.(d(k).eq.d(k+1))) then
	close(20,status='delete')
	 endif
	endif
 enddo

1000 continue
	if(k.eq.1) then
	if(d(1).eq.d(2)) then
		if(count1(1).ge.count1(2)) then
			print *,'1',d(1),count1(1)
		endif
	endif
	endif
	
	if(k.ge.2) then
	if((d(k).eq.d(k+1))) then
		if(count1(k).ge.count1(k+1)) then
			print *,k,d(k),count1(k),h(k)
		endif
	endif
	if(d(k).eq.d(k-1)) then
		if(count1(k).ge.count1(k-1)) then
			print *,k,d(k),count1(k),h(k)
		endif
	endif
	endif





n=0
 read(30,'()')
 do
 	read(30,*,end=200) filen
 	n=n+1
 enddo
200 continue

 rewind(30)
 read(30,'()')
 do k=1,n
 	dd(k)=0
 	read(30,*) filen
 	print *,filen
	day2 = filen(15:17)
	read(day2,*) dd(k)
enddo

 rewind(30)
 read(30,'()')
	

 do k=1,n
 	dd(k)=0
 	read(30,*) filen
 	print *,filen
	day2 = filen(15:17)
	read(day2,*) dd(k)
 
	 
	open(40,file=filen,status='old',form='unformatted',access='direct',recl=2)
	irec=0
	max=0
	count2(k)=0
	do j=1,y
	 do i=1,x
	  irec=irec+1
	  read(40,rec=irec) ref
	  if(ref.ge.max) max=ref
	  if(ref.eq.-1) count2(k)=count2(k)+1
	 ! print *,max
	 enddo
	enddo
	if(count2(k).ge.x*y*0.1) then
	 print *,count2(k)
	 if((dd(k).eq.dd(k-1)).or.dd(k).eq.dd(k+1)) then
	 close(40,status='delete')
	endif
	else
	 close(40)
	endif
	!close(40) 
	
 enddo
	if(dd(1).eq.dd(2)) then
		if(count2(1).ge.count2(2)) then
			print *,'1',dd(1),count2(1)
		endif
	endif


do k=2,n
	if((dd(k).eq.dd(k+1))) then
		if(count2(k).ge.count2(k+1)) then
			print *,k,dd(k),count2(k)
		endif
	endif
	if(dd(k).eq.dd(k-1)) then
		if(count2(k).ge.count2(k-1)) then
			print *,k,dd(k),count2(k)
		endif
	endif
enddo




end program
