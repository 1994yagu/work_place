program roatation

  implicit none


  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  integer :: i,j,k
  integer :: n,irec
  integer  :: yy1,yy2,dd1,dd2,mm1,mm2,hh1,hh2
  integer,dimension(:),allocatable :: yr,mn,hr
  integer :: sum(gx,gy)
  real :: rr,dx,dy,dv
  real :: u(gx,gy),v(gx,gy),sok(gx,gy),r(gx,gy),rot(gx,gy)
  real :: divx(gx,gy),divy(gx,gy),div(gx,gy)
  real :: time
  character :: filen*16,name*16,day*12,iyear*64
  character*64 :: fname1,fname2,ifile,ofile
  character*2 :: m1,m2,h1,h2,win_scale
  character*3 :: d1,d2
  character*4 :: y1,y2

! open list file

  iyear='201404_05'

  open(10,file=trim(adjustl(iyear))//'/cul_file1.txt',status='old')
  open(20,file=trim(adjustl(iyear))//'/cul_file2.txt',status='old')


  n=0
  do
     read(10,*,end=100) filen
     n=n+1
  enddo
100 continue

  print *,n

  rewind(10)

!  n=1

  do k=1,n
     read(10,*) fname1
     read(20,*) fname2

!     fname1= '2014215.2215.dat'
!     fname2= '2014215.2335.dat'

     write(win_scale,'(i2)') m

     ifile=fname1(1:12)
     ofile=fname1(1:15)//'.txt'

     y1=fname1(1:4)
     y2=fname2(1:4)
     d1=fname1(5:7)
     d2=fname2(5:7)
     h1=fname1(9:10)
     h2=fname2(9:10)
     m1=fname1(11:12)
     m2=fname2(11:12)

     read(y1,*) yy1
     read(y2,*) yy2
     read(d1,*) dd1
     read(d2,*) dd2
     read(h1,*) hh1
     read(h2,*) hh2
     read(m1,*) mm1
     read(m2,*) mm2

     if(dd2.eq.dd1) then
        time=(real(hh2)*60.+real(mm2))*60.-(real(hh1)*60.+real(mm1))*60.
     else
        time=(real(hh2)*60.+real(mm2))*60.+((23.-real(hh1))*60.+(60.-real(mm1)))*60.
     endif
     
     if((time.le.3600).or.(time.ge.10800)) cycle

     open(70,file=trim(adjustl(iyear))//'/'//trim(adjustl(ifile))//'_'//win_scale//'.dat' &
          & ,form='unformatted',recl=4,access='direct',status='old')
     open(90,file='rot/'//trim(adjustl(ifile))//'_rot.txt',status='replace')
     
     irec=0
     do j=1,gy
        do i=1,gx
           irec=irec+1
           read(70,rec=irec) u(i,j)
           if((u(i,j).eq.999.).or.(u(i,j).eq.-999.)) u(i,j)=999.
           irec=irec+1
           read(70,rec=irec) v(i,j)
           if((v(i,j).eq.999.).or.(v(i,j).eq.-999.)) v(i,j)=999.
           irec=irec+1
           read(70,rec=irec) sok(i,j)
        enddo
     enddo


     do j=1,gy
        do i=1,gx

           rr=0
           dx=0
           dy=0
           dv=0
           
           if((i.eq.1).or.(i.eq.gx).or.(j.eq.1).or.(j.eq.gy)) then
              rr=999.
              dx=999.
              dy=999.
              dv=999.
           endif

           if((u(i-1,j).ne.999).and.(u(i+1,j).ne.999).and.(v(i,j-1).ne.999).and.(v(i,j+1).ne.999)) then
              rr=(v(i+1,j)-v(i-1,j))/250*10*2.-(u(i,j+1)-u(i,j-1))/250*10*2.
              dx=(u(i+1,j)-u(i-1,j))/250*10*2.
              dy=(v(i,j+1)-v(i,j-1))/250*10*2.
              dv=dx+dy
           else
              rr=999.
              dx=999.
              dy=999.
              dv=999.
           endif

           r(i,j)=rr
           divx(i,j)=dx
           divy(i,j)=dy
           div(i,j)=dv

           write(90,*) r(i,j),div(i,j)

           if(rr.ne.999) then
              rot(i,j)=rot(i,j)+rr
              sum(i,j)=sum(i,j)+1
           endif

        enddo
     enddo
     

  enddo

  open(30,file='srot'//trim(adjustl(iyear))//'.txt',status='replace')

  do j=1,gy
     do i=1,gx
        
        if((i.eq.1).or.(i.eq.gx).or.(j.eq.1).or.(j.eq.gy)) then
           rot(i,j)=999.        
        elseif(sum(i,j).eq.0) then
           rot(i,j)=999.
        else
           rot(i,j)=rot(i,j)/real(sum(i,j))
        endif
        write(30,*) rot(i,j)
     enddo
  enddo

endprogram roatation
