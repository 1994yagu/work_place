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
