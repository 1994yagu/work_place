program read 

  implicit none
  integer i,j,irec
  integer*2 IC1(900,900),IC2(900,900)
  real IC(900,900)
  integer :: syr,tyr,smn,tmn,sdy,tdy,gy,gm,gd,ed
  character*2 :: mm,dd
  character*4 :: yy
  character*64 file1,file2,file3

  syr=2014
  tyr=syr
  smn=3
  tmn=smn
  sdy=1
  tdy=sdy

  gy=2014
  gm=10
  gd=31

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

     write(yy,'(I0.4)') tyr
     write(mm,'(I0.2)') tmn
     write(dd,'(I0.2)') tdy

     !GW1AM220141226A_IC0NP.dat

     file1='GW1AM2'//yy//mm//dd//'A_IC0NP.dat'
     file2='GW1AM2'//yy//mm//dd//'D_IC0NP.dat'
     file3=yy//mm//dd//'_IC0NPi.dat'


     open(10,file=file1,recl=2,status='old',form='unformatted',access='direct')
     open(20,file=file2,recl=2,status='old',form='unformatted',access='direct')
     open(30,file=file3,status='replace',form='unformatted',access='direct',recl=4)


     irec=0


     do j=1,900
        do i=1,900
           irec=irec+1
           read(10,rec=irec) IC1(i,j)
           read(20,rec=irec) IC2(i,j)
           if((IC1(i,j).ge.0).and.(IC1(i,j).le.100).and.(IC2(i,j).lt.0).and.(IC2(i,j).gt.100)) IC(i,j)=real(IC1(i,j))
           if((IC1(i,j).lt.0).and.(IC1(i,j).gt.100).and.(IC2(i,j).ge.0).and.(IC2(i,j).le.100)) IC(i,j)=real(IC2(i,j))
           if((IC1(i,j).ge.0).and.(IC1(i,j).le.100).and.(IC2(i,j).ge.0).and.(IC2(i,j).le.100)) &
                IC(i,j)=(real(IC1(i,j))+real(IC2(i,j)))/2.
           if((IC1(i,j).gt.-9000).and.(IC1(i,j).lt.0).and.(IC2(i,j).gt.-9000).and.(IC2(i,j).lt.0)) IC(i,j)=-888.
           if((IC1(i,j).le.-9000).and.(IC2(i,j).le.-9000)) IC(i,j)=-999.
           
           print *,IC(i,j)
        enddo
     enddo


     irec=0
     do j=1,900
        do i=1,900
           irec=irec+1
           write(30,rec=irec) IC(i,j)
        enddo
     enddo





     close(10)
     close(20)
     close(30)

     if((tyr.eq.gy).and.(tdy.eq.gd).and.(tmn.eq.gm)) then
        exit
     else
        tdy=tdy+1
     endif

  enddo

end program read
