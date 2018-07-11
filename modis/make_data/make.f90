!MODISの連続観測データ（5分後）を合成するプログラム
!xとyは範囲によって変更

program make_composit

  implicit none
  integer, parameter :: x=888
  integer, parameter :: y=888
  integer :: i,j,k,n,irec,ll
  integer*2 :: ref,ref1,ref2,ref3
  integer :: max
  character :: file1
  character*64 :: filen,mfile
  character(64),dimension(:),allocatable :: name
  character :: hour*2,min*2,day*3
  integer,allocatable,dimension(:) :: h,m,d

  ! modisのlistを読み出し
  open(10,file='tfile.txt',status='old')
  open(20,file='afile.txt',status='old')

  !listを順番に読み出し
  do ll=10,20,10

     !処理するデータ数をカウント
     n=0
     do
 	read(ll,*,end=100) filen
 	n=n+1
     enddo
100  continue
     rewind(ll)

     !     print *,n

     !配列の割り振り
     allocate(name(n))
     allocate(h(n))
     allocate(m(n))
     allocate(d(n))


     !処理の開始
     !日時を各変数に格納
     do k=1,n
 	h(k)=0
	m(k)=0
	d(k)=0
 	read(ll,*) filen
	name(k) = filen
	day = filen(15:17)
	hour = filen(19:20)
	min = filen(21:22)
	read(day,*) d(k)
	read(hour,*) h(k)
	read(min,*) m(k)

     enddo

     !合成するファイル化の確認
     do k=2,n
        if(d(k-1).eq.d(k)) then
           if((h(k)*60+m(k)-h(k-1)*60-m(k-1)).eq.5)then
              
              !３連続のファイルの場合
              if(((h(k-1)*60+m(k-1)-h(k-2)*60-m(k-2)).eq.5).and.(k.ge.3)) then
                 
                 open(30,file=name(k-2),status='old',recl=2,form='unformatted',access='direct') 
                 open(40,file=name(k-1),status='old',recl=2,form='unformatted',access='direct')
                 open(50,file=name(k),status='old',recl=2,form='unformatted',access='direct')
                 open(60,file='ref/'//name(k),status='replace',form='unformatted',access='direct',recl=2)
                 
                 irec=0
                 do j=1,y
                    do i=1,x
                       irec=irec+1
                       read(30,rec=irec) ref1
                       read(40,rec=irec) ref2
                       read(50,rec=irec) ref3
                       
                       if(ref1.ge.0) then
                          ref=ref1
                       elseif(ref2.ge.0) then
                          ref=ref2
                       elseif(ref3.ge.0) then
                          ref=ref3
                       else
                          ref=-1
                       endif
                       
89                     continue
                       
                       write(60,rec=irec) ref
                       
                       
                    enddo
                 enddo
                 
                 close(30)
                 close(40)
                 close(50)
                 close(60)
                 
              else
                 
                 !２連続のファイルの場合
                 
                 open(30,file=name(k-1),status='old',recl=2,form='unformatted',access='direct') 
                 open(40,file=name(k),status='old',recl=2,form='unformatted',access='direct')
                 open(60,file='ref/'//name(k),status='replace',form='unformatted',access='direct',recl=2)
                 
                 print *,name(k-1),name(k)
                 irec=0
                 do j=1,y
                    do i=1,x
                       irec=irec+1
                       read(30,rec=irec) ref1
                       read(40,rec=irec) ref2
                       
                       if(ref1.ge.0) then
                          ref=ref1
                       elseif(ref2.ge.0) then
                          ref=ref2
                       else
                          ref=-1
                       endif
                       
                       write(60,rec=irec) ref
                       
                       
                    enddo
                 enddo
                 
                 close(30)
                 close(40)
                 close(60)
                 
              endif
              
              
              
           elseif((h(k+1)*60+m(k+1)-h(k)*60-m(k)).ne.5) then
              
              open(30,file=name(k),status='old',recl=2,form='unformatted',access='direct') 
              open(60,file='ref/'//name(k),status='replace',form='unformatted',access='direct',recl=2)
              
              irec=0
              do j=1,y
                 do i=1,x
                    
                    irec=irec+1
                    read(30,rec=irec) ref
                    write(60,rec=irec) ref
                    
                 enddo
              enddo
              
              close(30)
              close(60)
              
           endif

        else
           !直前の日付と異なる場合
           !５分差
           if((h(k)*60+m(k)+24*60-h(k-1)*60-m(k-1)).eq.5) then

              open(30,file=name(k),status='old',recl=2,form='unformatted',access='direct')
              open(40,file=name(k),status='old',recl=2,form='unformatted',access='direct')
              open(60,file='ref/'//name(k),status='replace',form='unformatted',access='direct',recl=2)

              irec=0
              do j=1,y
                 do i=1,x

                    irec=irec+1
                    read(30,rec=irec) ref1
                    read(40,rec=irec) ref2
                       
                    if(ref1.ge.0) then
                       ref=ref1
                    elseif(ref2.ge.0) then
                       ref=ref2
                    else
                       ref=-1
                    endif
                    
                    write(60,rec=irec) ref
                    
                 enddo
              enddo

              close(30);close(40);close(60)
           else
              !５分差出ない場合
              open(30,file=name(k),status='old',recl=2,form='unformatted',access='direct')
              open(60,file='ref/'//name(k),status='replace',form='unformatted',access='direct',recl=2)
              do j=1,gy
                 do i=1,gx
                    irec=irec+1
                    read(30,rec=irec) ref                            
                    write(60,rec=irec) ref
                 enddo
              enddo
              close(30);close(60)
           endif
	endif
enddo

enddo



end program make_composit
