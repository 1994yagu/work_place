#!/bin/sh
# Swath MOD03 (MODIS Geolocation) flies (500m resolution)
# 2008/01/17 Y.Oyama
# usage: swath_angle.sh

outpath=/Users/yaguchi/HDD2/northpole/2003/03/out/
prmpath=/Users/yaguchi/HDD2/northpole/2003/
prmconf=north_rap.prm

for fname in MYD02QKM.*.hdf;
do swath2grid -if=${fname} -gf=${fname} \
-of=${outpath}${fname%.hdf} -pf=${prmpath}${prmconf}

done

for fname in MOD02QKM.*.hdf;
do swath2grid -if=${fname} -gf=${fname} \
-of=${outpath}${fname%.hdf} -pf=${prmpath}${prmconf}


done
