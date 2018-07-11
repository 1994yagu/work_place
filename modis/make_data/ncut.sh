#!/bin/sh


echo '' > file.txt

m=0
for f in MO*.dat
do 
	echo $f
	name=`echo $f | cut -c 1-22`
	echo ${name}.dat >> file.txt
	mv $f ${name}.dat 
	m=m+1
done
echo $m

echo '' > file1.txt

for f in MY*.dat
do 
	echo $f
	name=`echo $f | cut -c 1-22`
	echo ${name}.dat >> file1.txt
	mv $f ${name}.dat 
done