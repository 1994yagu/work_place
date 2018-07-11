program wind_factor

  implicit none
  integer,parameter :: X=888 ! original grid of x
  integer,parameter :: Y=888 ! original grid of y
  integer,parameter :: wx=48 ! outside window of x
  integer,parameter :: wy=48 ! outside window of y
  integer,parameter :: m=20 ! window scale
  integer,parameter :: gx=(X-wx)/m ! drift speed grid
  integer,parameter :: gy=(y-wy)/m ! drift speed grid
  integer :: i,j,nf,irec,k,wi,wj,kf,rf
  integer  :: yy1,yy2,dd1,dd2,mm1,mm2,hh1,hh2
  integer :: month,nday,jday,year
  integer,dimension(1:gx,1:gy) :: sumu,sumv
  integer :: time
  real,allocatable,dimension(:,:,:) :: du,dv,sok,udi,vdi,ludi,lvdi
  real,dimension(1:gx,1:gy) :: asumu,asumv,swu,swv,wsum,awsumu,awsumv,sdu,sdv
  real,allocatable,dimension(:,:,:) :: wu,wv,wvel
  real,allocatable,dimension(:) :: winu,winv
  real,dimension(1:gx,1:gy) :: suu,suv,svu,svv,su2,sv2
  real,dimension(1:gx,1:gy) :: seta,F
  real,dimension(1:gx,1:gy) :: c1,c2,c3,c4
  real,parameter :: pi=4.0d0*datan(1.0d0)
  character*10 :: tyear
  character*64 :: dfile1,dfile2
  character*64 :: fname1,fname2,ifile,ofile
  character*80 :: wfile
  character*2 :: m1,m2,h1,h2,win_scale,fy
  character*3 :: d1,d2
  character*4 :: y1,y2
  character*2 :: nd,mon
  character(20),allocatable,dimension(:) :: dname
  real :: refw,sscuw,sscvw,cocow,xmu2,xmv2,xmu,ymu,xmv,ymv
  real :: sxu,syu,sxv,syv,sxxu,syyu,sxxv,syyv,sxyuu,sxyvv,sxyuv,sxyvu
  real :: sita,cc,ccc,coco,gradw,ref,ss,sscu,sscv,sss,ssxyu,ssxyv,grad
  real :: sxyu,the,tst,tue
  integer :: count,countw
  integer :: mi,mj
  real :: fmin
  real :: sgradw,srefw,scocow,scountw,asum
  real ::   magradw,marefw,macocow,macountw,migradw,mirefw,micocow,micountw
  integer :: smonth
  character*6 :: fmonth
  real :: avedu,avedv,avewu,avewv,aasum


  tyear='201408_09'
  fmonth='201408'
  smonth=9 !skip month
  
  dfile1='/Users/yaguchi/HDD2/northpole/moor/con/'//trim(adjustl(tyear))//'/cul_file1.txt'
  dfile2='/Users/yaguchi/HDD2/northpole/moor/con/'//trim(adjustl(tyear))//'/cul_file2.txt'

  open(11,file=dfile1,status='old')
  open(21,file=dfile2,status='old')

  open(33,file='seta.txt',status='replace')
!  open(44,file='ave.txt',status='replace')
  open(66,file=trim(adjustl(fmonth))//'.txt',status='replace')
  open(77,file='winm.txt',status='replace')
  open(88,file='ave.txt',status='replace')
  
  nf=0
  do
     read(11,*,end=55)
     nf=nf+1
  enddo

 55 continue
  
  rewind(11)

  kf=0

  do
     read(21,*,end=56)
     kf=kf+1
  enddo
  
56 continue
  
  rewind(21)

!  print *,nf,kf

!  stop

  allocate(du(gx,gy,nf))
  allocate(dv(gx,gy,nf))
  allocate(sok(gx,gy,nf))
  allocate(wu(X,Y,nf))
  allocate(wv(X,Y,nf))
  allocate(wvel(X,Y,nf))
  allocate(udi(X,Y,nf))
  allocate(vdi(X,Y,nf))
  allocate(ludi(X,Y,nf))
  allocate(lvdi(X,Y,nf))
  allocate(winu(nf))
  allocate(winv(nf))
  allocate(dname(nf))

  rf=0
  
  do k=1,nf
     read(11,*) fname1
     read(21,*) fname2

     dname(k)=fname1
     

     write(win_scale,'(i2)') m

     ifile=fname1(1:12)

     fy=fname1(3:4)
     
     y1=fname1(1:4)
     y2=fname2(1:4)
     d1=fname1(5:7)
     d2=fname2(5:7)
     h1=fname1(9:10)
     h2=fname2(9:10)
     m1=fname1(11:12)
     m2=fname2(11:12)

 !    print *,d1

     read(y1,*) yy1
     read(y2,*) yy2
     read(d1,*) dd1
     read(d2,*) dd2
     read(h1,*) hh1
     read(h2,*) hh2
     read(m1,*) mm1
     read(m2,*) mm2

     year=yy1
     jday=dd1
     
     call cal_nday(year,jday,month,nday)

     write(mon,'(i2.2)') month
     write(nd,'(i2.2)') nday

     if (month.eq.smonth) then
        cycle
     endif

     rf=rf+1

     print *,month,nday

     if(dd2.eq.dd1) then
        time=(real(hh2)*60.+real(mm2))*60.-(real(hh1)*60.+real(mm1))*60.
     else
        time=(real(hh2)*60.+real(mm2))*60.+((23.-real(hh1))*60.+(60.-real(mm1)))*60.
     endif
          
     if((time.le.3600).or.(time.ge.10800)) cycle
  

     open(70,file='/Users/yaguchi/HDD2/northpole/moor/con/'//trim(adjustl(tyear))//'/' &
          & //trim(adjustl(ifile))//'_'//win_scale//'.dat',form='unformatted',recl=4,access='direct',status='old')

     irec=0
     do j=1,gy
        do i=1,gx
           
           irec=irec+1
           read(70,rec=irec) du(i,j,rf)
           
           if((i.eq.25).and.(j.eq.30)) then
              if(du(i,j,rf).eq.0) print *,fname1
           endif
           
           if(du(i,j,rf).ne.999) then
              sdu(i,j)=sdu(i,j)+du(i,j,rf)
              sumu(i,j)=sumu(i,j)+1
           endif
           
           irec=irec+1
           read(70,rec=irec) dv(i,j,rf)
           if(dv(i,j,rf).ne.999) then
              sdv(i,j)=sdv(i,j)+dv(i,j,rf)
              sumv(i,j)=sumv(i,j)+1
           endif
           
           irec=irec+1
           read(70,rec=irec) sok(i,j,rf)
           
        end do
     end do


     
     if ((hh1.ge.0).and.(hh1.lt.6)) then
        wfile='win_moor/'//y1//mon//'/'//fy//mon//nd//'00_win.dat'
     endif

     if ((hh1.ge.6).and.(hh1.lt.12)) then
        wfile='win_moor/'//y1//mon//'/'//fy//mon//nd//'06_win.dat'
     endif

     if ((hh1.ge.12).and.(hh1.lt.18)) then
        wfile='win_moor/'//y1//mon//'/'//fy//mon//nd//'12_win.dat'
     endif

     if ((hh1.ge.18).and.(hh1.lt.24)) then
        wfile='win_moor/'//y1//mon//'/'//fy//mon//nd//'18_win.dat'
     endif

     open(71,file=wfile,recl=4,status='old',form='unformatted',access='direct')
     
     irec=0
     do j=1,888
        do i=1,888
           irec=irec+1
           read(71,rec=irec) wu(i,j,rf)
           wu(i,j,rf)=wu(i,j,rf)*100.

!           wi=20*i+24
!           wj=20*j+24
!           if((wi.eq.25).and.(wj.eq.30)) then
!              if(wu(wi,wj,rf).eq.0) print *,y1,d1,h1
!           endif
              
           irec=irec+1
           read(71,rec=irec) wv(i,j,rf)
           wv(i,j,rf)=wv(i,j,rf)*100.
           irec=irec+1
           read(71,rec=irec) wvel(i,j,rf) 
        enddo
     enddo
     
!     print *,k


!     do j=1,gy
!        do i=1,gx
!           print *,i,j
!           wi=20*i+24
!           wj=20*j+24
!           print *,wi,wj
!           if((du(i,j,k).ne.999.).or.(dv(i,j,k).ne.999.)) then
!              swu(i,j)=wu(wi,wj,k)+swu(i,j)
!              swv(i,j)=wv(wi,wj,k)+swv(i,j)
!              wsum(i,j)=wsum(i,j)+1
!           endif
           
!        enddo
!     enddo

     

  end do
  
  sgradw=0
  srefw=0
  scocow=0
  scountw=0
  asum=0
  magradw=-999.
  marefw=-999.
  macocow=-999.
  macountw=-999.
  migradw=999
  mirefw=999
  micocow=999
  micountw=999
  avedu=0
  avedv=0
  avewu=0
  avewv=0
  aasum=0

  do j=1,gy
     do i=1,gx

        refw=0.
        sscuw=0.
        sscvw=0.
        cocow=-1.
        xmu2=0.
        xmv2=0.
        xmu=0.
        ymu=0.
        xmv=0.
        ymv=0.
        sxu=0.
        syu=0.
        sxv=0.
        syv=0.
        sxxu=0.
        syyu=0.
        sxxv=0.
        syyv=0.
        sxyuu=0.
        sxyvv=0.
        sxyuv=0.
        sxyvu=0.
        count=0
        sita=0.
        
        do k=1,rf
           if((du(i,j,k).ne.999).and.(dv(i,j,k).ne.999)) then
              count=count+1
              wi=20*i+24
              wj=20*j+24
              winu(k)=wu(wi,wj,k)                           
              winv(k)=wv(wi,wj,k)
              ymu=ymu+du(i,j,k)
              ymv=ymv+dv(i,j,k)
              xmu=xmu+wu(wi,wj,k)
              xmv=xmv+wv(wi,wj,k)
              avedu=avedu+du(i,j,k)
              avedv=avedv+dv(i,j,k)
              avewu=avewu+wu(i,j,k)
              avewv=avewv+wv(i,j,k)
              aasum=aasum+1
           else
              cycle
           endif
        enddo

        if(count.le.10) then
           gradw=999.
           refw=999.
           sscuw=999.
           sscvw=999.
           cocow=999.
           countw=0
           ymu=999.
           ymv=999.
           goto 53
        endif
        
        xmu=xmu/real(count)
        xmv=xmv/real(count)
        ymu=ymu/real(count)
        ymv=ymv/real(count)
        
        do k=1,rf
           if((du(i,j,k).ne.999).and.(dv(i,j,k).ne.999)) then
              sxu=sxu+(winu(k)-xmu)
              sxv=sxv+(winv(k)-xmv)
              syu=syu+(du(i,j,k)-ymu)
              syv=syv+(dv(i,j,k)-ymv)
              sxxu=sxxu+(winu(k)-xmu)**2
              sxxv=sxxv+(winv(k)-xmv)**2
              syyu=syyu+(du(i,j,k)-ymu)**2
              syyv=syyv+(dv(i,j,k)-ymv)**2
              sxyuu=sxyuu+(winu(k)-xmu)*(du(i,j,k)-ymu)
              sxyvv=sxyvv+(winv(k)-xmv)*(dv(i,j,k)-ymv)
              sxyuv=sxyuv+(winu(k)-xmu)*(dv(i,j,k)-ymv)
              sxyvu=sxyvu+(winv(k)-xmv)*(du(i,j,k)-ymu)
              sita=sita+sqrt((winu(k)-xmu)**2+(winv(k)-xmv)**2)* &
                   & sqrt((du(i,j,k)-ymu)**2+(dv(i,j,k)-ymv)**2)
           endif
           
        enddo
        
        tue=sxyuv-sxyvu
        tst=sxyuu+sxyvv
        
        if(tst.eq.0) tst=0.000001
!        the=atan(tue/tst)
        the=atan2(tue,tst)
!        grad=atan2d(tue,tst)
        if(the.ge.pi/2.) print *,the,tue,tst,the*180/pi
        
!        if((tst.ge.0.).and.(tue.ge.0)) the=the
!        if((tst.lt.0.).and.(tue.ge.0)) the=the
!        if((tst.lt.0.).and.(tue.lt.0)) the=2.*pi+the
!        if((tst.ge.0.).and.(tue.lt.0)) the=2.*pi+the
        
        ss=sin(the)
        cc=cos(the)
        
        ccc=cc**2
        sss=ss*2
        
        
        ref=(cc*sxyuu-ss*sxyvu+ss*sxyuv+cc*sxyvv)/(sxxu+sxxv)
        sscu=ymu-ref*cc*xmu-ref*ss*xmv
        sscv=ymv+ref*ss*xmu-ref*cc*xmv
        
        ssxyu=sqrt(sxxu*syyu)
        ssxyv=sqrt(sxxv*syyv)
        
        if((ssxyu.eq.0.).or.(ssxyv.eq.0.)) then
           coco=-1.
        else
           coco=(cc*sxyuu-ss*sxyvu+ss*sxyuv+cc*sxyvv)/sqrt((sxxu+sxxv)*(syyu+syyv))
        endif
        
        cocow=coco
        refw=ref
        
        gradw=the*180./pi
        countw=count
        sscuw=sscu
        sscvw=sscv

!        if(refw.lt.0) then
           
!           if(gradw.ge.0) then
!              gradw=180.-gradw
!              refw=-1.*refw
!              gradw=gradw-360.
!           else
!              gradw=180.+gradw
!              refw=-1.*refw
!              gradw=gradw+360.
!           endif

!        endif
              

53      continue

        if(gradw.ne.999) then
           sgradw=gradw+sgradw
           srefw=srefw+refw
           scocow=scocow+cocow
           scountw=scountw+countw
           asum=asum+1

           if(magradw.lt.gradw) magradw=gradw
           if(migradw.gt.gradw) migradw=gradw
           if(marefw.lt.refw) marefw=refw
           if(mirefw.gt.refw) mirefw=refw
           if(macocow.lt.cocow) macocow=cocow
           if(micocow.gt.cocow) micocow=cocow
           if(macountw.lt.countw) macountw=countw
           if(micountw.gt.countw) micountw=countw
           
        endif
        
        write(66,*) gradw,refw,cocow,countw
        write(88,*) xmu,xmv,ymu,ymv

!        if (refw.le.fmin) then
!           fmin=refw
!           mi=i
!           mj=j
        !        endif

        if((i.eq.25).and.(j.eq.30)) then
           do k=1,rf
              if(du(i,j,k).ne.999) write(77,*) k,du(i,j,k),dv(i,j,k),winu(k),winv(k),trim(adjustl(dname(k)))
           enddo
        endif
        
     enddo
  enddo


!  print *,mi,mj,fmin
  print *,nf,rf

  print *,sgradw/asum,srefw/asum,scocow/asum,scountw/asum
  print *,magradw,marefw,macocow,macountw
  print *,migradw,mirefw,micocow,micountw
  print *,avedu/aasum,avedv/aasum,avewu/aasum,avewv/aasum


        
end program wind_factor
  
