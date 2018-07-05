program modis_convert
  implicit none

  integer :: i,j,ihem,irec,res,itrans
  integer :: x,y,ii,jj
  real :: lat,lon,ai,aj,ai1,ai2,aj1,aj2
  real :: li,lj
  real :: llon(888,888),llat(888,888)

  open(20,file='vlon.txt',status='replace')

  ihem=1
  res=40
  
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

  print *,ai1,aj1,ai2,aj2

  if(ai1.ge.ai2) then
     li=ai1
  else
     li=ai2
  endif
  
  if(aj1.ge.aj2) then
     lj=aj1
  else
     lj=aj2
  endif
  
  res=40

  do y=888,1,-1
     do x=888,1,-1
        i=li-(888-x)
        j=lj-(888-y)

        call ij2latlon(i,j,ihem,res,lat,lon)

        write(20,*) lat,lon
     end do
  end do

  close(20)

  ! moor lat=74.51972222,lon=-163.56083333

  lat=73.52669722
  lon=-161.93310833 
  
  call latlon2ij(lat,lon,ihem,res,ai,aj)

  print *, li-ai,lj-aj
!  print * , ((li-ai)-5.)/20., ((lj-aj)-5.)/20. !UL
  print * , ((li-ai)-24.)/20., ((lj-aj)-24.)/20. !center

  open(30,file='vlon.txt',status='old')
  open(40,file='veclon.txt',status='replace')

  do j=1,888
     do i=1,888
        read(30,*) llat(i,j),llon(i,j)
     enddo
  enddo
  
  
  do j=1,(888-48)/20
     do i=1,(888-48)/20
!        ii=24+(i-1)*20+1!UL
!        jj=24+(j-1)*20+1!UL
        ii=24+(i-1)*20+1+19 !center
        jj=24+(j-1)*20+1+19 !center
        write(40,*) llat(ii,jj),llon(ii,jj)
        if((i.eq.1).and.(j.eq.1)) print *,llat(ii,jj),llon(ii,jj)
        if((i.eq.(888-48)/20).and.(j.eq.(888-48)/20)) print *,llat(ii,jj),llon(ii,jj)
     enddo
  enddo

  close(30)
  close(40)
  
        
  
  
  
  
end program modis_convert

        
