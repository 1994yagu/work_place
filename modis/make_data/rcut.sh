#!/bin/sh


echo '' > rfile.txt

m=0
for f in rMO*
do 
	echo $f
	name=`echo $f |cut -c 12-23`
	mv $f ${name}.dat
done
echo $m

echo '' > rfile1.txt

for f in rMY*.dat
do 
	echo $f
	name=`echo $f | cut -c 12-23`
	mv $f ${name}.dat 
done

for f in 2014*.dat
do
	echo $f
	echo $f >> rfile.txt
	echo $f >> rfile1.txt
done
