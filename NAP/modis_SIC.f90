!program modis grid to lat lon to amsr grid
! modis上の緯度経度からAMSRグリッドを探査し、SICデータを取得

program modis_amsr
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  integer :: ami(gx,gy),amj(gx,gy)
  integer :: i,j,ihem,irec,res,itrans
  integer*2 :: ii,jj
  integer :: syr,tyr,smn,tmn,sdy,tdy,fy,fm,fd,ed
  real :: AIC(900,900)
  real :: lat,lon,ai,aj,ai1,ai2,aj1,aj2
  real :: li,lj
  real :: llon(gx,gy),llat(gx,gy)
  real :: IC
  character*100 :: mod_posfile,amr_posfile,in_ICfile,out_ICfile
  character*2 :: mm,dd,ayr
  character*4 :: yy

  mod_posfile='/Users/yaguchi/HDD2/data/NAP_area/moor/veclon.txt' !NAP範囲の緯度経度データ
  amr_posfile='/Users/yaguchi/HDD2/data/NAP_area/moor/NAP_amsr_point.txt' !NAP範囲のAMSRグリッド

  open(10,file=mod_posfile,status='old') 
  !  open(20,file=amr_posfile,status='replace',form='unformatted',recl=2,access='direct') !バイナリ
  open(20,file=amr_posfile,status='replace')
!  open(50,file='/Users/yaguchi/HDD2/data/amsr/amsr_lon_lat.txt')

  !test amsr

!  do j=1,900
!     do i=1,900
!        ihem=1
!        res=1
!        call ij2latlon(i,j,ihem,res,lat,lon)
!        write(50,*) lat,lon
!     enddo
!  enddo
        

  !NAP範囲の緯度経度データを読み出して、NAPのAMSRグリッドを読み出し
  
  irec=0
  do j=1,gy
     do i=1,gx
        
        lat=0
        lon=0
        read(10,*) lat,lon
        ihem=1
        res=1
        call latlon2ij(lat,lon,ihem,res,ai,aj)
        ii=int(ai)
        jj=int(aj)
!        print *,ii,jj
        !irec=irec+1
        !write(20,rec=irec) ii
        !irec=irec+1
        !write(20,rec=irec) jj
        write(20,*) 901-ii,jj
        ami(i,j)=901-ii
        amj(i,j)=901-jj
        
     enddo
  enddo
  close(10)
  close(20)

  !切り出し終了

  !AMSRデータの読み込み

  !読み込み開始年月日
  syr=2014
  smn=3
  sdy=1
  
  tyr=syr
  tmn=smn
  tdy=sdy

  !読み込み終了年月日
  fy=2014
  fm=10
  fd=31

  do

!　年月日の文字列化
     
     write(yy,'(I0.4)') tyr
     ayr=yy(3:4)
     write(mm,'(I0.2)') tmn
     write(dd,'(I0.2)') tdy
     
     !　海氷密接度データの取得
     
     in_ICfile='/Users/yaguchi/HDD2/data/AMSR/NPi/'//ayr//mm//dd//'_IC0NPi.dat'
     out_ICfile='/Users/yaguchi/HDD2/data/NAP_area/amsr/drift/'//ayr//mm//dd//'dri_IC0NPi.dat'
     open(30,file=in_ICfile,status='old',recl=4,form='unformatted',access='direct')
     open(40,file=out_ICfile,status='replace',recl=4,form='unformatted',access='direct')
     

     !海氷密接度データの読み出し
     irec=0
     do j=1,900
        do i=1,900
           IC=0
           irec=irec+1
           read(30,rec=irec) IC
           AIC(901-i,j)=IC
        enddo
     enddo

     !NAP漂流速度地点でのSICの切り出し
     irec=0
     do j=1,gy
        do i=1,gx
           irec=irec+1
           write(40,rec=irec) AIC(ami(i,j),amj(i,j))
        enddo
     enddo

     close(30);close(40)
     
           
!設定終了日にループを抜ける
     if((tyr.eq.fy).and.(tdy.eq.fd).and.(tmn.eq.fm)) then
        exit
     endif


     
  !年月日の移動
     call cal_day(tyr,tmn,tdy,tyear,tmonth,tday)

  enddo
  

  

end program modis_amsr
  

  
