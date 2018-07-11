program read_draft_NAP13
  implicit none
  integer ,parameter :: num=28770925
  integer :: k,irec,kk,m
  integer :: year,month,day,hour,min,ed,sum,scon
  integer :: tday(num)
  real*4 :: draft
  real*4 :: adraft
  real :: icon
  real*8 :: t,sec
  character*4 :: yy
  character*2 :: mm,dd,mi,hh
  character*64 :: i_dir,o_dir
  
  !  2013 9 22 0 0 0.034

  o_dir='/Users/yaguchi/HDD2/out/NAP_area/draft/'
  i_dir='/Users/yaguchi/HDD2/data/NAP_area/moor/'

  open(30,file=trim(adjustl(i_dir))//'CanadaBasin_1314_draft_FINAL.dat',access='direct',form='unformatted',status='old',recl=4)
  open(40,file=trim(adjustl(o_dir))//'draft_NAP.txt',status='replace')
  open(50,file=trim(adjustl(o_dir))//'draft_dat.txt',status='replace') !all draft data
  open(60,file=trim(adjustl(o_dir))//'draft_ave.txt',status='replace') !draft average
  open(70,file=trim(adjustl(o_dir))//'draft_cha.txt',status='replace') !chenge point draft
  open(80,file=trim(adjustl(o_dir))//'moor_cha_con.txt',status='replace')

  !日付の指定
  t=1.0000096
  year=2013
  month=9
  day=22
  hour=0
  min=0
  sec=0.034

  kk=0

  irec=0
  sum=0
  adraft=0.
  m=0
  icon=0.
  do k=1,num
     
     irec=irec+1
     read(30,rec=irec) draft !draftデータの読み込み
     adraft=adraft+draft !合計値の計算
     sum=sum+1

     !正時が変わる時のドラフトの平均を記録
     if((sec.ge.30).and.(min.eq.59)) then
        if(draft.ne.0) then
           icon=icon+1.
        endif
        scon=scon+1.        
     elseif((sec.le.30).and.(min.eq.0)) then
!        print *,draft
        if(draft.ne.0) then
           icon=icon+1.
        endif
        scon=scon+1.
     endif
     
     tday(k)=day

     !     print *,sec,min,day,hour,month,year

     !日付を文字列へ変換
     
     write(yy,'(I0.4)') year
     write(mm,'(I0.2)') month
     write(dd,'(I0.2)') day
     write(hh,'(I0.2)') hour
     write(mi,'(I0.2)') min

     !8月分を出力
     if((year.eq.2014).and.(month.ge.7)) then
        kk=kk+1
        write(40,*) kk,draft
        if((k.ge.2).and.(tday(k).ne.tday(k-1))) then
           write(50,*) kk,0,mm,'/',dd
        endif
     endif

     if((min.eq.0).and.(sec.le.1)) then
        write(70,*) yy,' ',mm,' ',dd,' ',hh,' ',draft
    !    print *,yy,'',mm,'',dd,'',hh
     endif

     if((min.eq.0).and.(sec.gt.30).and.(sec.lt.31)) then
        if((year.eq.2014).and.(month.eq.8)) then
!           print *,icon,scon
           icon=icon/real(scon)
           write(80,*) yy,' ',mm,' ',dd,' ',hh,' ',icon
         endif
         icon=0.
         scon=0
     endif

     sec=sec+t
 
        
     if(sec.ge.60.) then
        min=min+1
        sec=sec-60.

        adraft=adraft/real(sum)
        if((year.eq.2014).and.(month.ge.7)) then
           m=m+1
           write(60,*) m,adraft,dd,kk
        endif
        adraft=0.
        sum=0


        if(min.eq.60) then
           hour=hour+1
           min=min-60          
           if(hour.eq.24) then
              day=day+1
              hour=hour-24
              if((month.eq.1).or.(month.eq.3).or.(month.eq.5).or.(month.eq.7).or.(month.eq.10) &
                   .or.(month.eq.12).or.(month.eq.8)) then
                 ed=31
              elseif((month.eq.4).or.(month.eq.6).or.(month.eq.9).or.(month.eq.11)) then
                 ed=30
              else
                 if(mod(year,4).eq.0) then
                    ed=29
                 else
                    ed=28
                 endif
              endif
              if(day.gt.ed) then
                 day=day-ed
                 month=month+1
                 if(month.eq.13)then
                    month=1
                    year=year+1

                 endif
              endif
           endif
        endif
     endif
  enddo
end program read_draft_NAP13
