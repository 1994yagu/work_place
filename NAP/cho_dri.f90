program select_drift
  implicit none
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  integer :: i,j,k,irec,ii,jj,dcon
  real,dimension(1:gx,1:gy) :: du,dv,sok
  real :: mx,my
  real :: uu,vv,fr,sum,ll,cvel,minll,nv,nu
  character*64 :: ifile

  ifile='201408_09/2014218.2110_20.dat'

  open(11,file=ifile,status='old',access='direct',form='unformatted',recl=4)

  ii=765
  jj=105

  irec=0
  do j=1,gy
     do i=1,gx
        irec=irec+1
        read(11,rec=irec) du(i,j)
        irec=irec+1
        read(11,rec=irec) dv(i,j)
        irec=irec+1
        read(11,rec=irec) sok(i,j)
     enddo
  enddo

  minll=10
  
  do j=1,gy
     do i=1,gx
        mx=(ii-24)/20
        my=(jj-24)/20
        ll=sqrt((mx-real(i))**2+(my-real(j))**2)
        if(minll.ge.ll) then
           minll=ll
           nu=du(i,j)
           nv=dv(i,j)
        endif
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

  print *,uu,vv
  print *,nu,nv
end program select_drift
