subroutine last_day(tyr,tmn,ed)
  integer tyr,tmn
  integer ed

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
end subroutine last_day
