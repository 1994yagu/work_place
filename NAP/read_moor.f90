program read_moor_original
  implicit none
  integer,parameter :: num=109792
  integer :: k,irec
  real*8 :: a,b,c,d,e
  character*64 :: ifile
  
  ifile='CanadaBasin_1314_dist_ed03_seg02.dat'

  open(30,file=ifile,access='direct',form='unformatted',status='old',recl=8)
  open(40,file='test.txt',status='replace')

  irec=0
  do k=1,num
     irec=irec+1
     read(30,rec=irec) a
     irec=irec+1
     read(30,rec=irec) b
     irec=irec+1
     read(30,rec=irec) c
     irec=irec+1
     read(30,rec=irec) d
     irec=irec+1
     read(30,rec=irec) e

     write(40,*) a,b,c,d,e
  enddo
end program read_moor_original
