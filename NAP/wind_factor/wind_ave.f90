program wind_factor

  implicit none
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  real,allocatable,dimension(:,:,:) :: wu,wv,wvel
  real,allocatable,dimension(:,:) :: swu,swv,swvel,sum
  integer :: n,k,i,j,irec
  character*64 :: s_dir,o_dir,filen,wname
  

  s_dir='/Users/yaguchi/HDD2/data/wind_north/moor_area/201408'
  o_dir='/Users/yaguchi/HDD2/out/wind/NAP_area/'

  open(10,file=trim(adjustl(s_dir))//'/wind_list.txt',status='old')

  n=0
  do
     read(10,*,end=100) filen
     n=n+1
  enddo
100 continue

  print *,n

  rewind(10)

  allocate(wu(X,Y,n))
  allocate(wv(X,Y,n))
  allocate(wvel(X,Y,n))
  allocate(swu(X,Y))
  allocate(swv(X,Y))
  allocate(swvel(X,Y))
  allocate(sum(X,Y))
  
  do k=1,n
     read(10,*) wname

     open(70,file=trim(adjustl(s_dir))//'/'//trim(adjustl(wname)),form='unformatted',recl=4,access='direct',status='old')

     irec=0
     do j=1,888
        do i=1,888
           irec=irec+1
           read(70,rec=irec) wu(i,j,k)
           wu(i,j,k)=wu(i,j,k)*100.
           swu(i,j)=wu(i,j,k)+swu(i,j)
           irec=irec+1
           read(70,rec=irec) wv(i,j,k)
           wv(i,j,k)=wv(i,j,k)*100.
           swv(i,j)=wv(i,j,k)+swv(i,j)
           irec=irec+1
           read(70,rec=irec) wvel(i,j,k)
           swvel(i,j)=wvel(i,j,k)+swvel(i,j)
           sum(i,j)=sum(i,j)+1
        enddo
     enddo

     close(70)

  enddo

  open(67,file=trim(adjustl(o_dir))//'201408_win_ave.txt',status='replace')

  do j=1,888
     do i=1,888
        swu(i,j)=swu(i,j)/real(sum(i,j))
        swv(i,j)=swv(i,j)/real(sum(i,j))
        swvel(i,j)=swvel(i,j)/real(sum(i,j))
        write(67,*) swu(i,j),swv(i,j),swvel(i,j)
     enddo
  enddo
  close(67)
  
end program wind_factor
