!海氷密接度と渦度の関係

program compare_rotation_SIC
  implicit none
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  integer :: i,j,k,tt
  integer :: n,irec
  integer  :: yy1,yy2,dd1,dd2,mm1,mm2,hh1,hh2
  integer,dimension(:),allocatable :: yr,mn,hr
  integer :: jday,mon1,mon2,day1,day2
  integer :: sum(gx,gy),ssum
  integer :: smonth
  integer :: month,nday,year
  real :: rr,dx,dy,dv
  real :: u(gx,gy),v(gx,gy),sok(gx,gy),r(gx,gy),rot(gx,gy)
  real :: divx(gx,gy),divy(gx,gy),div(gx,gy),ic(gx,gy)
  real :: time
  character :: filen*16,name*16,day*12,iyear*64
  character*10 :: tyear
  character*64 :: fname1,fname2,ifile,ofile
  character*2 :: m1,m2,h1,h2,win_scale
  character*3 :: d1,d2
  character*4 :: y1,y2
  character*2 :: i_year,i_mon,i_day
  character*6 :: fmonth
  character*2 :: tmonth
  ! open list file

  open(10,file='/Users/yaguchi/HDD2/out/NAP_area/rot/rot_list.txt',status='old')

  !読み込むファイルの数を取得
  n=0
  do
     read(10,*,end=100) filen
     n=n+1
  enddo
100 continue

  !  print *,n
  do tt=3,8
     smonth=tt !target month
     write(tmonth,'(i2.2)') smonth

     rewind(10)

     !対応
     open(21,file='/Users/yaguchi/HDD2/out/NAP_area/rot/nap_ic_'//trim(adjustl(tmonth))//'.txt',status='replace')
     open(22,file='/Users/yaguchi/HDD2/out/NAP_area/rot/nap_rot_'//trim(adjustl(tmonth))//'.txt',status='replace')
     open(23,file='/Users/yaguchi/HDD2/out/NAP_area/rot/ic_rot_'//trim(adjustl(tmonth))//'.txt',status='replace')

     !  n=1

     !漂流速度ファイルを取得・読み込み

     ssum=0

     do k=1,n

        !ファイル名の読み出し

        read(10,*) ifile

        !渦度・発散の読み込み

        open(70,file='/Users/yaguchi/HDD2/out/NAP_area/rot/'//trim(adjustl(ifile)),status='old')

        !2014091.0025_rot.txt

        y1=ifile(1:4)
        d1=ifile(5:7)
        h1=ifile(9:10)
        m1=ifile(11:12)

        read(y1,*) yy1
        read(d1,*) dd1
        read(h1,*) hh1
        read(m1,*) mm1

        do j=1,gy
           do i=1,gx
              !           print *,i,j
              read(70,*) rot(i,j),div(i,j)
           enddo
        enddo

        close(70)

        !ジュリアンデイから普通の日付へ換算

        year=yy1
        jday=dd1    
        call cal_nday(year,jday,month,nday)
        mon1=month
        day1=nday

        i_year=y1(3:4)
        write(i_mon,'(i2.2)') month
        write(i_day,'(i2.2)') nday

        if (month.ne.smonth) then
           cycle
        endif

        !140916dri_IC0NPi.dat
        !NAPエリアの海氷密接度の読み込み
        open(80,file='/Users/yaguchi/HDD2/data/NAP_area/amsr/drift/'//i_year//i_mon//i_day//'dri_IC0NPi.dat' &
             & ,status='old',form='unformatted',recl=4,access='direct')

        irec=0
        do j=1,gy
           do i=1,gx
              irec=irec+1
              read(80,rec=irec) ic(i,j)
              !          print *,ic(i,j)
           enddo
        enddo

        close(80)


        do j=1,gy
           do i=1,gx 
              if(rot(i,j).ne.999) then
                 ssum=ssum+1
                 write(21,*) ic(i,j)
                 write(22,*) rot(i,j)
                 write(23,*) ic(i,j),rot(i,j)
              endif
           enddo
        enddo

     enddo
     print *,smonth,ssum

     close(21)
     close(22)
     close(23)

  enddo
end program compare_rotation_SIC
