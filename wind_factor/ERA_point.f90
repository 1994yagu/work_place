!with compile amsr2_converter.f90

program ERA_point
  implicit none
  integer :: i,j,nf,k,ii,jj
  integer :: ihem,res,irec
  real :: lon,lat,wv,wu
  real ::ai,aj,ait,ajt
  real :: ai1,ai2,aj1,aj2,li,lj
  real :: v(888,888),u(888,888),sum(888,888)
  real ::fr,ll
  real*4 :: ru,rv,vel,tu,tv
  real*8 :: pi,seta

  open(10,file='u10.txt',status='old')
  open(11,file='v10.txt',status='old')

  open(20,file='u10_amsr.txt',status='replace')
  open(21,file='v10_amsr.txt',status='replace')
  !  open(33,file='modis_lon_lat.txt')
  open(23,file='win10_modis.dat',status='replace',form='unformatted',access='direct',recl=4)

  ihem=1
  res=40
  pi=4.0d0*datan(1.0d0)

! calculate region
  
  lat=74.887
  lon=-158.482

  call latlon2ij(lat,lon,ihem,res,ai,aj)

  ai1=ai
  aj1=aj

    
  lat=73.542
  lon=-167.718
  

  call latlon2ij(lat,lon,ihem,res,ai,aj)

  ai2=ai
  aj2=aj

!  print *,ai1,aj1,ai2,aj2

  if(ai1.lt.ai2) then
     li=ai1
  else
     li=ai2
  endif
  
  if(aj1.lt.aj2) then
     lj=aj1
  else
     lj=aj2
  endif

!  print *,ai1,ai2,aj1,aj2,li,lj
  
  
  
  nf=0
  do
     read(10,*,end=55)
     nf=nf+1
  enddo

55 continue

  rewind(10)

  do k=1,nf
     read(10,*) lon,lat,wv
     read(11,*) lon,lat,wu
     
     call latlon2ij(lat,lon,ihem,res,ai,aj)

 !    if((ai.ge.1).and.(ai.le.36000).and.(aj.ge.1).and.(aj.le.36000)) then
 !       
 !    endif

!     print *,ai,aj,li,lj

     

     if((ai.gt.li-100.).and.(ai.lt.li+999.).and.(aj.gt.lj-100.).and.(aj.lt.lj+999.)) then


        ait=ai-li
        ajt=aj-lj
!        print *,ait,ajt
!        write(33,*) ait,ajt

        do j=1,888
           do i=1,888
              ll=sqrt((ait-real(i))**2+(ajt-real(j))**2)
              if(ll.le.100) then
!                 print *,ll
                 fr=exp((2.*(-1*(ll**2.))/72.**2.))
                 v(i,j)=v(i,j)+wv*fr
                 u(i,j)=u(i,j)+wu*fr
                 sum(i,j)=sum(i,j)+fr
              endif
           enddo
        enddo
     end if

  enddo

  open(33,file='vlon.txt',status='old')

  do j=1,888
     do i=1,888
        v(889-i,889-j)=v(889-i,889-j)/sum(889-i,889-j)
        u(889-i,889-j)=u(889-i,889-j)/sum(889-i,889-j)
        write(20,*) i,j,v(889-i,889-j)
        write(21,*) i,j,u(889-i,889-j)        
     enddo
  enddo

  close(20)
  close(21)

!  open(40,file='u10_amsr.txt',status='old')
!  open(41,file='v10_amsr.txt',status='old')

!  open(44,file='w10_m.txt',status='replace')
  
  irec=0
  do j=1,888
     do i=1,888
        !        read(40,*) ii,jj,wu
        !        read(41,*) ii,jj,wv

        tu=u(889-i,889-j)
        tv=v(889-i,889-j)
        read(33,*) lat,lon
!        print *,lon4x
        seta=-lon*pi/180.
!        print *,seta
        rv=-tv*sin(seta)+tu*cos(seta)
        ru=tv*cos(seta)+tu*sin(seta)
        vel=sqrt(ru**2+rv**2)
        
        irec=irec+1
        write(23,rec=irec) ru
        irec=irec+1
        write(23,rec=irec) rv
        irec=irec+1
        write(23,rec=irec) vel
!        print *,ru,rv,vel

!        write(44,*) ru,rv,vel
     end do
  enddo
  
end program ERA_point
