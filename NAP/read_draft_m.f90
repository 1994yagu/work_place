program read_moor_original
  implicit none
  integer,parameter :: num=3871674
  integer :: k,irec
  real*8 :: a,b,c,d,e
  character*100 :: ifile,o_dir,s_dir,ofile

  o_dir='/Users/yaguchi/HDD2/out/NAP_area/draft/'
  s_dir='/Users/yaguchi/HDD2/data/NAP_area/moor/'
  
  ifile=trim(adjustl(s_dir))//'CanadaBasin_1314_dist_ed03_seg04.dat'
  ofile=trim(adjustl(o_dir))//'CanadaBasin_1314_dist_ed03_seg04.txt'
  
  open(30,file=ifile,access='direct',form='unformatted',status='old',recl=8)
  open(40,file=ofile,status='replace')

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
