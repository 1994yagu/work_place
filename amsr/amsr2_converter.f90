program write_IC

  integer :: i,j,irec
  integer ihem,res
  real :: lat,lon
  real :: IC(900,900)
  character*64 :: ifile,ofile
  integer :: syr,tyr,smn,tmn,sdy,tdy,gy,gm,gd,ed
  character*2 :: mm,dd
  character*2:: yy

  syr=14
  tyr=syr
  smn=3
  tmn=smn
  sdy=1
  tdy=sdy

  gy=14
  gm=08
  gd=31

!  lat=74.51972222
  !  lon=-163.56083333

!  print *,'input '

!  call latlon2ij(lat,lon,ihem,res,ai,aj)

!  exit

  
  do

     if((tmn.eq.1).or.(tmn.eq.3).or.(tmn.eq.5).or.(tmn.eq.7).or.(tmn.eq.10) &
          .or.(tmn.eq.12).or.(tmn.eq.8)) then
        ed=31
     elseif((tmn.eq.4).or.(tmn.eq.6).or.(tmn.eq.9).or.(tmn.eq.11)) then
        ed=30
     else
        if(mod(tyr,4).eq.0) then
           ed=29
        else
           ed=28
        endif
     endif

     if(tdy.gt.ed) then
        tdy=1
        tmn=tmn+1
        if(tmn.ge.13) tyr=tyr+1
     endif

     write(yy,'(I0.2)') tyr
     write(mm,'(I0.2)') tmn
     write(dd,'(I0.2)') tdy

     ifile=yy//mm//dd//'_IC0NPi.dat'
     ofile=yy//mm//dd//'_lim_IC0NPi.dat'

     open(12,file=ifile,form='unformatted',access='direct',status='old',recl=4)
     open(13,file=ofile,form='unformatted',access='direct',status='replace',recl=4)

     ihem = 1
     res=1

     irec=0
     do j=1,900
        do i=1,900
           irec=irec+1
           read(12,rec=irec) IC(901-i,j)
!           call  ij2latlon(i,j,ihem,res,lat,lon)
        enddo
     enddo

!     call latlon2ij(lat,lon,ihem,res,ai,aj)
     irec=0
     do j=901-298,901-275
        do i=901-511,901-488
           !do j=1,900
           ! do i=1,900
           irec=irec+1
           write(13,rec=irec) IC(i,j)
        enddo
     enddo
     
     

     close(10)
     close(20)

     if((tyr.eq.gy).and.(tdy.eq.gd).and.(tmn.eq.gm)) then
        exit
     else
        tdy=tdy+1
     endif

  enddo
     
   end program write_IC






   subroutine ij2latlon(i,j,ihem,res,lat,lon)
     !------------------------------------------------------
     ! grid converter [ grid number -> lat-lon coordinate ]
     !   i,j     : input grid number
     !   ihem    : for select hemisphere
     !             = 1 (north pole)
     !             = 2 (south pole)
     !   res     : for select grid size
     !             = 1 (nx=900,ny=900)
     !             = 2 (nx=1800,ny=1800) [only for 89H,89V]
     !   lat,lon : output coordinate
     !------------------------------------------------------
     implicit none
     integer, intent(in)  :: i, j, ihem, res
     real,    intent(out) :: lat, lon

     integer :: dir

     real :: PI
     real :: deg2rad, rad2deg
     real :: r0, phi0, rate
     real :: di, dj
     real :: alpha, a2, phi

     real :: xc
     real :: yc
     real :: dlat = 54.36

     xc = ( 900 * res + 1 ) * 0.5
     yc = ( 900 * res + 1 ) * 0.5

     if( ihem == 1 ) then
        dir = 1
     else
        dir = -1
     end if

     PI = 4.0 * atan(1.0)
     deg2rad = PI/180.0
     rad2deg = 180.0/PI

     r0 = sqrt((xc-1)*(xc-1)+(yc-1)*(yc-1))
     phi0 = dlat * deg2rad
     rate = r0 * (1.0+cos(phi0)) / sin(phi0);

     di = xc - real(i);
     dj = real(j) - yc;

     alpha = sqrt(di*di+dj*dj) / rate
     a2 = alpha * alpha;
     phi = acos( (1.0-a2)/(1.0+a2) )

     lon = atan2(dir*di,dj) * rad2deg
     lat = dir * ( 90.0 - phi * rad2deg )

     if( lon > 180.0 ) then
        lon = lon - 360.0
     end if
   end subroutine ij2latlon


   subroutine latlon2ij(lat,lon,ihem,res,ai,aj)
     !------------------------------------------------------
     ! grid converter [ lat-lon coordinate -> grid number ]
     !   lat,lon : output coordinate
     !   ihem    : for select hemisphere
     !             = 1 (north pole)
     !             = 2 (south pole)
     !   res     : for select grid size
     !             = 1 (nx=900,ny=900)
     !             = 2 (nx=1800,ny=1800) [only for 89H,89V]
     !   ai,aj   : output grid number   [NOTICE:data type is REAL]
     !------------------------------------------------------
     implicit none
     real,    intent(in)  :: lat, lon
     integer, intent(in)  :: ihem, res
     real,    intent(out) :: ai, aj

     integer :: dir

     real :: PI
     real :: deg2rad, rad2deg
     real :: r0, phi0, rate
     real :: lam, phi
     real :: r

     real :: xc
     real :: yc
     real :: dlat = 54.36

     xc = ( 900 * res + 1 ) * 0.5
     yc = ( 900 * res + 1 ) * 0.5

     if( ihem == 1 ) then
        dir = 1
     else
        dir = -1
     end if

     PI = 4.0 * atan(1.0)
     deg2rad = PI/180.0
     rad2deg = 180.0/PI

     r0 = sqrt((xc-1)*(xc-1)+(yc-1)*(yc-1))
     phi0 = dlat * deg2rad
     rate = r0 * (1.0+cos(phi0)) / sin(phi0);

     phi = ( 90 - dir * lat ) * deg2rad
     lam = lon * deg2rad

     r = rate * sin(phi) / max(1+cos(phi),1.e-8)

     ai = xc - r * sin(lam) * dir
     aj = yc + r * cos(lam)

   end subroutine latlon2ij
