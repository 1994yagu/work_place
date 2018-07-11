#!/bin/sh

ori_dir='/Users/yaguchi/HDD2/work_place/modis/make_data'
s_dir='/Users/yaguchi/HDD2/data/NAP_area/201507_08/bin'
o_dir=${s_dir}'data'

[ ! -e $o_dir ] && mkdir -p $o_dir

cd ${s_dir}

cat /dev/null > tfile.txt

for f in MO*.dat
do 
	echo $f
	name=`echo $f | cut -c 1-22`
	echo ${name}.dat >> tfile.txt
	mv $f ${name}.dat 
done

cat /dev/null > afile.txt

for f in MY*.dat
do 
	echo $f
	name=`echo $f | cut -c 1-22`
	echo ${name}.dat >> file1.txt
	mv $f ${name}.dat 
done

cd ${ori_dir}

gfortran make.f90 -o make-bin-o

mv make-bin-o ${s_dir}

cd ${sdir}

./make-bin-o

rm make-bin-o

cd ${ori_dir}
