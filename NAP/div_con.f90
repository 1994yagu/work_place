program roatation

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
  integer,dimension(:),allocatable :: yr,mn,dy,hr
  real,dimension(:),allocatable :: u,v,dep,con
  real,dimension(1:gx,1:gy) :: du,dv,sok,vel,divu,divv,div
  integer :: i,j,irec,k,n,l,ii,jj,nf
  integer :: k1,k2,kk,count
  integer  :: yy1,yy2,dd1,dd2,mm1,mm2,hh1,hh2
  integer :: month,year,nday,time
  integer :: jday,mon1,mon2,day1,day2
  real :: uu,vv,fr,sum,ll
  real,dimension(2) :: ux,vy
  real :: mdiv,mdivu,mdivv
  real :: ru,rv,rru,rrv,de,pde
  character*64 :: fname1,fname2,ifile,ofile,sdate
  character*4 :: y1,y2
  character*2 :: d1,d2,m1,m2,h1,h2,win_scale,no

  sdate='201406_07'

  open(10,file=trim(adjustl(sdate))//'/cul_file1.txt',status='old')
  open(20,file=trim(adjustl(sdate))//'/cul_file2.txt',status='old')

  open(30,file='NAP13t_IPS_hourly_kimura.txt',status='old')
  open(40,file=trim(adjustl(sdate))//'div.txt',status='replace')

  nf=0
  do
     read(10,*,end=50)
     nf=nf+1
  enddo
50 continue
  
  k=0
  do
     read(30,*,end=55)
     k=k+1
  enddo
55 continue

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
!     print *,dep(kk)
  enddo
  

  print *,nf

  rewind(10) 

  do n=1,nf

!     print *,n
     read(10,*) fname1
     read(20,*) fname2

     write(win_scale,'(i2)') m

     ifile=fname1(1:12)
     ofile=fname1(1:15)//'.txt'

!     print *,ifile
     
     y1=fname1(1:4)
     y2=fname2(1:4)
     d1=fname1(5:7)
     d2=fname2(5:7)
     h1=fname1(9:10)
     h2=fname2(9:10)
     m1=fname1(11:12)
     m2=fname2(11:12)

!     print *,y1

     read(y1,*) yy1
     read(y2,*) yy2
     read(d1,*) dd1
     read(d2,*) dd2
     read(h1,*) hh1
     read(h2,*) hh2
     read(m1,*) mm1
     read(m2,*) mm2

     if(dd2.eq.dd1) then
        time=(real(hh2)*60.+real(mm2))*60.-(real(hh1)*60.+real(mm1))*60.
     else
        time=(real(hh2)*60.+real(mm2))*60.+((23.-real(hh1))*60.+(60.-real(mm1)))*60.
     endif

     !     print *,time

     if((time.le.3600).or.(time.ge.7200)) cycle

     open(70,file=trim(adjustl(sdate))//'/'//trim(adjustl(ifile))//'_'//win_scale//'.dat',form='unformatted', &
          recl=4,access='direct',status='old')

     !     write(no,'(i2)') n

     open(77,file='div'//trim(adjustl(ofile)),status='replace')


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

     do j=1,gy
        do i=1,gx

           if((i.eq.1).or.(i.eq.gx).or.(j.eq.1).or.(j.eq.gy)) then
              div(i,j)=999.
           else
              if((du(i-1,j).ne.999).and.(du(i+1,j).ne.999.)) then
                 divu(i,j)=(du(i-1,j)-du(i+1,j))/250.*10.*2
              else
                 divu(i,j)=999.
              endif

              if((dv(i,j-1).ne.999).and.(du(i,j+1).ne.999.)) then
                 divv(i,j)=(dv(i,j-1)-dv(i,j+1))/250.*10.*2
              else
                 divv(i,j)=999.
              endif

              if((divu(i,j).ne.999).and.(divv(i,j).ne.999)) then
                 div(i,j)=divu(i,j)+divv(i,j)
              else
                 div(i,j)=999.

              endif

           endif

           write(77,*) div(i,j)

        enddo
     enddo

     l=0
     do ii=-1,1,2

        l=l+1
        uu=0
        vv=0
        sum=0
        fr=0
        count=0

        do j=int(my)-5,int(my)+5
           do i=int(mx)-5,int(mx)+5

              ll=sqrt((mx+ii-real(i))**2+(my-real(j))**2)

              if(ll.le.1.8) then
                 if((abs(du(i,j)).le.800).and.(abs(dv(i,j)).le.800)) then
                    fr=exp((2.*(-1*(ll**2.))/1.2**2.))
                    uu=uu+du(i,j)*fr
                    sum=sum+fr
                    count=count+1
                 endif
              endif



           enddo
        enddo

        if(count.ge.3) then
           ux(l)=uu/sum
           print *,count
        else
           ux(l)=999.
        endif

     enddo

     if((ux(1).ne.999).and.(ux(2).ne.999)) then
        mdivu=(ux(2)-ux(1))/(250.*10.*100.*2)
     else
        mdivu=999.
     endif

     l=0
     do jj=-1,1,2

        l=l+1
        uu=0
        vv=0
        sum=0
        fr=0
        count=0

        do j=int(my)-5,int(my)+5
           do i=int(mx)-5,int(mx)+5

              ll=sqrt((mx-real(i))**2+(my+jj-real(j))**2)

              if(ll.le.1.8) then
                 if((abs(du(i,j)).le.800).and.(abs(dv(i,j)).le.800)) then
                    fr=exp((2.*(-1*(ll**2.))/1.2**2.))
                    uu=uu+du(i,j)*fr
                    sum=sum+fr
                 endif
              endif



           enddo
        enddo

        if(count.ge.0) then
           vy(l)=uu/sum
        else
           vy(l)=999.
        endif

     enddo

     if((vy(1).ne.999).and.(vy(2).ne.999)) then
        mdivv=(vy(2)-vy(1))/(250.*10.*100.*2)
     else
        mdivv=999.
     endif

     if((mdivu.ne.999).and.(mdivv.ne.999)) then
        mdiv=mdivu+mdivv
     else
        mdiv=999.
     endif

 !    print *,mdiv

     !search moor data

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

     k1=0
     do kk=1,k
        if((yr(kk).eq.yy1).and.(mn(kk).eq.mon1).and.(dy(kk).eq.day1).and.(hr(kk).eq.hh1)) then

           k1=kk
 !          print *,yr(kk),mn(kk),dy(kk),hr(kk),dep(kk)
           exit
        endif
     enddo


     rewind(30)

     k2=0
     do kk=1,k
        if((yr(kk).eq.yy2).and.(mn(kk).eq.mon2).and.(dy(kk).eq.day2).and.(hr(kk).eq.hh2)) then

           k2=kk
           exit
        endif
     enddo

     de=0
     pde=0
     irec=0

     do kk=k1,k2

        irec=irec+1
        de=de+dep(kk)

     enddo
     
     pde=dep(k1)-dep(k2)
     print *,dep(k1),dep(k2),k1,k2
     de=de/real(irec)

     if(div(8,34).ne.999) then
        write(40,*) pde,de,div(8,34)
     endif
     



  enddo

endprogram roatation

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
           else
              ed=29
           endif
        endif
     endif

     if(nday.ge.ed) then

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

