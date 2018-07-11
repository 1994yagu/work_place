subroutine cal_day(tyr,tmn,tdy,tyear,tmonth,tday)
  integer :: ii,ed
  integer :: tyr,tmn,tdy
  integer :: tyear,tmonth,tday

  tdy=tdy+1

  if((tmn.eq.1).or.(tmn.eq.3).or.(tmn.eq.5).or.(tmn.eq.7).or.(tmn.eq.10) &
       .or.(tmn.eq.12).or.(tmn.eq.8)) then
     ed=31
  elseif((tmn.eq.4).or.(tmn.eq.6).or.(tmn.eq.9).or.(tmn.eq.11)) then
     ed=30
  else
     if((mod(tyr,400).EQ.0).or.((mod(tyr,4).EQ.0).and.(mod(tyr,100).NE.0)))then
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

  tyear=tyr
  tmonth=tmn
  tyr=tyear


end subroutine cal_day
