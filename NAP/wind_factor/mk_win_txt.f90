program divide_win

  implicit none
  integer :: i,j,irec
  real :: u,v,vel
  character*64 ifile

  ifile='win10_modis.dat'
  
  open(11,file=ifile,status='old',form='unformatted',access='direct',recl=4)
  open(12,file='cccc.txt',status='replace')
  
  irec=0
  do j=1,888
     do i=1,888
        irec=irec+1
        read(11,rec=irec) u
        irec=irec+1
        read(11,rec=irec) v
        irec=irec+1
        read(11,rec=irec) vel

        write(12,*) u,v,vel
     enddo
  enddo

end program divide_win
