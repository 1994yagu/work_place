program cpmpare_moor
  implicit none
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  real,parameter :: mx=8.13144493 ! moor x position
  real,parameter :: my=34.0167961 ! moor y position
  integer :: i,j,irec,k,n,l,nf,ccon,dcon,idis
  integer :: k1,k2,kk
  character*64 :: fname1,fname2,ifile,ofile
  integer  :: yy1,yy2,dd1,dd2,mm1,mm2,hh1,hh2
  integer,dimension(:),allocatable :: yr,mn,dy,hr
  integer :: jday,mon1,mon2,day1,day2
  integer :: month,year,nday
  real,dimension(:),allocatable :: u,v,dep,con,tu,tv
  real,dimension(1:gx,1:gy) :: du,dv,sok,vel
  real :: ru,rv,rru,rrv,rvel,cvel
  real :: time,lon
  real*8 :: pi
  real :: uu,vv,fr,sum,ll,ic,udis,vdis
  character*2 :: m1,m2,h1,h2,win_scale
  character*3 :: d1,d2
  character*4 :: y1,y2
  real :: seta
 

  pi=4.0d0*datan(1.0d0)
  lon=(180-161.926531666666667)*pi/180.
  lon=-161.926531666666667*pi/180.
  print *,lon,cos(lon),sin(lon)


  open(10,file='cul_file1.txt',status='old')
  open(20,file='cul_file2.txt',status='old')
  open(30,file='NAP13t_IPS_hourly_kimura.txt',status='old')
  open(40,file='compare.txt',status='replace')
  open(60,file='no_ice.txt',status='replace')
  open(31,file='var.txt',status='replace')
  open(90,file='moor_vel.txt',status='replace')

  nf=0
  do
     read(10,*,end=55)
     nf=nf+1
  enddo

55 continue
  
  rewind(10)

  
! count colume of moor file  

  k=0
  do
     read(30,*,end=50)
     k=k+1
  enddo

50 continue

! read moor date

  allocate( yr(1:k) )
  allocate( mn(1:k) )
  allocate( dy(1:k) )
  allocate( hr(1:k) )
  allocate( dep(1:k) )
  allocate( con(1:k) )
  allocate( u(1:k) )
  allocate( v(1:k) )

  rewind(30)
  
  do kk=1,k
     read(30,*) yr(kk),mn(kk),dy(kk),hr(kk),dep(kk),con(kk),u(kk),v(kk)
  enddo



  ccon=0

! calculate average velocity  
  
  do n=1,nf
     read(10,*) fname1
     read(20,*) fname2

     write(win_scale,'(i2)') m
     
     ifile=fname1(1:12)
     ofile=fname1(1:15)//'.txt'

!     2014060.2055.dat

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

     

     ! check_time_interval
     
     if(dd2.eq.dd1) then
        time=(real(hh2)*60.+real(mm2))*60.-(real(hh1)*60.+real(mm1))*60.
     else
        time=(real(hh2)*60.+real(mm2))*60.+((23.-real(hh1))*60.+(60.-real(mm1)))*60.
     endif
     
!     print *,time
     
     if((time.le.3600).or.(time.ge.10800)) cycle

! read drift speed
     
     open(70,file=trim(adjustl(ifile))//'_'//win_scale//'.dat',form='unformatted',recl=4,access='direct',status='old')

     irec=0
     do j=1,gy
        do i=1,gx
           irec=irec+1
           read(70,rec=irec) du(i,j)
           irec=irec+1
           read(70,rec=irec) dv(i,j)
           irec=irec+1
           read(70,rec=irec) sok(i,j)

           if((du(i,j).ne.999).or.(dv(i,j).ne.999)) then
              vel(i,j)=sqrt(du(i,j)**2+dv(i,j)**2)
           else
              vel(i,j)=999.
           endif
           
        end do
     end do

! average the drift speed around the moor 

     uu=0
     vv=0
     cvel=0
     sum=0
     dcon=0
     fr=0
     
     do j=1,gy
        do i=1,gx
           ll=sqrt((mx-real(i))**2+(my-real(j))**2)
           if(ll.le.1.8) then
              if((abs(du(i,j)).le.800).and.(abs(dv(i,j)).le.800)) then
                 !                 fr=exp((2.*(-1*(ll**2.))/1.2**2.))
                 fr=exp(-4.*(ll**2))
                 uu=uu+du(i,j)*fr
                 vv=vv+dv(i,j)*fr
                 cvel=cvel+sqrt(du(i,j)**2+dv(i,j)**2)*fr
                 sum=sum+fr
                 dcon=dcon+1
              endif
           endif
        enddo
     enddo

     if(fr.ne.0) then
        uu=uu/sum
        vv=vv/sum
        cvel=cvel/sum
     else
        uu=999
        vv=999
     endif

!     write(50,*) uu,vv


     ! approximate time

!     print *,dd1,hh1,mm1
     
     if(mm1.le.30) then
        mm1=0
        hh1=hh1
     else
        mm1=0
        hh1=hh1+1

        if(hh1.eq.24) then
           dd1=dd1+1
           hh1=0
        endif
     endif

!     print *,dd1,hh1,mm1

     if(mm2.le.30) then
        mm2=0
        hh2=hh2
     else
        mm2=0
        hh2=hh2+1

        if(hh2.eq.24) then
           dd2=dd2+1
           hh2=0
        endif
     endif

! convert julian day  to normal day
     year=yy1
     jday=dd1
     
     call cal_nday(year,jday,month,nday)
     mon1=month
     day1=nday

!     print *,mon1,day1
     
     year=yy2
     jday=dd2
     call cal_nday(year,jday,month,nday)
     mon2=month
     day2=nday
     

     
     ! search the object day

! start time
     
     k1=0
     do kk=1,k
        if((yr(kk).eq.yy1).and.(mn(kk).eq.mon1).and.(dy(kk).eq.day1).and.(hr(kk).eq.hh1)) then

           k1=kk
!           print *,u(kk)
           
           exit
        endif
     enddo


     rewind(30)

! end time     
     
     k2=0
     do kk=1,k
        if((yr(kk).eq.yy2).and.(mn(kk).eq.mon2).and.(dy(kk).eq.day2).and.(hr(kk).eq.hh2)) then

           k2=kk
           exit
        endif
     enddo

     ru=0
     rv=0
     rvel=0
     ic=0
     irec=0

     allocate( tu(k1:k2) )
     allocate( tv(k1:k2) )
  
     do kk=k1,k2

        irec=irec+1
        !        print *,u(kk)*100*cos(lon)-v(kk)*100*sin(lon),u(kk)*100*sin(lon)+v(kk)*100*cos(lon)
        !ru=ru+(v(kk)*100.*sin(lon)-u(kk)*100.*cos(lon))
        ru=ru+(-v(kk)*100.*sin(lon)+u(kk)*100.*cos(lon))
        !        rv=rv-(v(kk)*100.*cos(lon)+u(kk)*100.*sin(lon))
        rv=rv+(v(kk)*100.*cos(lon)+u(kk)*sin(lon))

        tu(kk)=v(kk)*100.*sin(lon)-u(kk)*100.*cos(lon)
        tv(kk)=-(v(kk)*100.*cos(lon)+u(kk)*100.*sin(lon))
        
        rvel=rvel+sqrt(v(kk)**2+u(kk)**2)*100.

        ic=ic+con(kk)
        
     enddo

! end calculate average velocity
     
     ru=ru/real(irec)
     rv=rv/real(irec)
     rvel=rvel/real(irec)
     ic=ic/real(irec)

     udis=0
     vdis=0
     idis=0
     
     do kk=k1,k2
        idis=idis+1
        udis=udis+(tu(kk)-ru)**2
        vdis=vdis+(tv(kk)-rv)**2
     enddo

     udis=udis/real(idis)
     vdis=vdis/real(idis)

    

     
     if ((uu.ne.999).or.(vv.ne.999)) then
        write(31,*) udis,vdis
        if(dcon.ge.3) then
           ccon=ccon+1
           write(40,*) uu,ru,vv,rv, trim(adjustl(fname1)),' ',trim(adjustl(fname2))
           if(ic.le.0.15) write(60,*) uu,ru,vv,rv,cvel,rvel

        endif
     end if

     seta=atan2(rv,ru)*180/pi

     
     write(90,*) mx,my,atan2(rv,ru)*180/pi,rvel/40.

     deallocate(tu)
     deallocate(tv)  
     
     
     
  enddo
  
     
  print *,ccon
  close(10)
  close(20)
  close(30)
  close(40)
  close(60)
  close(90)
     
end program cpmpare_moor



  
  
  
subroutine cal_nday(year,jday,month,nday)
  integer :: ii,ed
  integer :: year,jda,month,nday

  
  month=1
  nday=0
  do ii=1,jday
     
     nday=nday+1
     
     if((month.eq.1).or.(month.eq.3).or.(month.eq.5).or.(month.eq.7).or.(month.eq.8).or.(month.eq.10).or.(month.eq.12)) then
        ed=31
     elseif((month.eq.4).or.(month.eq.6).or.(month.eq.9).or.(month.eq.11)) then
        ed=30
     elseif(month.eq.2) then
        
        if(mod(year,4).eq.0) then
           
           if((mod(year,100).eq.0).and.(mod(year,400).eq.0)) then
              ed=29
           elseif((mod(year,100).eq.0).and.(mod(year,400).ne.0)) then
              ed=28
           endif
        else
           ed=28
        endif
     endif

     if(nday.gt.ed) then
        
        month=month+1
        nday=1

        if(month.ge.13) then
           year=year+1
           month=1
           nday=1
        endif
     endif
  enddo
  
end subroutine cal_nday
           
           
     
