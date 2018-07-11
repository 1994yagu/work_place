!program modis grid to lat lon to amsr grid
! amsrの海氷密接度の平均の計算

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
  integer :: sday,nm
  integer*2 :: ii,jj
  integer :: syr,tyr,smn,tmn,sdy,tdy,fy,fm,fd,ed
  real :: AIC(900,900,31),aveIC(900,900),SIC(900,900),sum(900,900)
  real :: lat,lon,ai,aj,ai1,ai2,aj1,aj2
  real :: li,lj
  real :: llon(gx,gy),llat(gx,gy)
  real :: IC
  character*100 :: mod_posfile,amr_posfile,in_ICfile,out_ICfile
  character*2 :: mm,dd,ayr
  character*4 :: yy

  !AMSRデータの読み込み

  !読み込み開始年月日
  syr=2014
  smn=5
  sdy=1

  tyr=syr
  tmn=smn
  tdy=sdy

  call last_day(tyr,tmn,ed)
  
  !読み込み終了年月日
  fy=2014
  fm=smn
  fd=ed

  sday=0 !各日のデータ入力

  write(yy,'(I0.4)') tyr 
  ayr=yy(3:4)
  write(mm,'(I0.2)') tmn

  out_ICfile='/Users/yaguchi/HDD2/data/NAP_area/amsr/ave_ic/'//ayr//mm//'ave_IC0NPi.dat'
  open(40,file=out_ICfile,status='replace',recl=4,form='unformatted',access='direct')

  do
     sday=sday+1

     !　年月日の文字列化

     write(yy,'(I0.4)') tyr
     ayr=yy(3:4)
     write(mm,'(I0.2)') tmn
     write(dd,'(I0.2)') tdy

     !　海氷密接度データの取得

     in_ICfile='/Users/yaguchi/HDD2/data/NAP_area/amsr/drift/'//ayr//mm//dd//'dri_IC0NPi.dat'
     open(30,file=in_ICfile,status='old',recl=4,form='unformatted',access='direct')



     !海氷密接度データの読み出し
     irec=0
     do j=1,gy
        do i=1,gx
           IC=0
           irec=irec+1
           read(30,rec=irec) IC
           AIC(i,j,sday)=IC
        enddo
     enddo

     close(30)


     !設定終了日にループを抜ける
     if((tyr.eq.fy).and.(tdy.eq.fd).and.(tmn.eq.fm)) then
        exit
     endif



     !年月日の移動
     call cal_day(tyr,tmn,tdy,tyear,tmonth,tday)

  enddo

  do nm=1,sday

     do j=1,gy
        do i=1,gx

           if((AIC(i,j,nm).ge.0).and.(AIC(i,j,nm).le.100)) then
              SIC(i,j)=SIC(i,j)+AIC(i,j,nm)
              sum(i,j)=sum(i,j)+1
           endif

        enddo
     enddo



     irec=0
     do j=1,gy
        do i=1,gx
           if(sum(i,j).ne.0) then
              aveIC(i,j)=SIC(i,j)/real(sum(i,j))
           else
              aveIC(i,j)=999.
           endif
           irec=irec+1
           write(40,rec=irec) aveIC(i,j)
        enddo
     enddo

  enddo



end program modis_amsr



