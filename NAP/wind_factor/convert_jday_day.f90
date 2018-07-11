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
           endif
        else
           ed=28
        endif
     endif

     if(nday.gt.ed) then
        
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
