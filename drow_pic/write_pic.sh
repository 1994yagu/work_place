#!bin/sh

range=1/42/1/42
MPRANGE=-158.973068/74.8304062/-167.494202/73.5827866
PDATA=moor_point.txt
P2DATA=moor_latlon.txt

makecpt -Cno_green -T0/40/0.1 -Z > vel.cpt

for f in *.dat

do

    ifile=`echo $f | cut -c 1-15`
    echo ${ifile}
    cp ${f} cccc.dat

    ./mkvec
    #awk '{if($3 <= 0){print $1,$2,"NaN"} else print $1,$2,$3}' $ifile > $icefile
    awk '{if($5 < 999){print $1,$2,$5} else print $1,$2,"NaN"}' cccc.txt > vel.txt
    awk '{if($3 < 999){print $1,$2,$3} else print $1,$2,"NaN"}' cccc.txt > u.txt
    awk '{if($4 < 999){print $1,$2,$4} else print $1,$2,"NaN"}' cccc.txt > v.txt
    
    #drow velocity
    xyz2grd vel.txt -R${range} -I1 -Gvel.grd
    #surface vel.txt -Gvel.grd -I0.2 -R${range}
    grdimage vel.grd -JX10c/10c -Q -Cvel.cpt -K -P > vec.ps

    #drow vector
    xyz2grd u.txt -R${range} -I1 -Gu.grd 
    xyz2grd v.txt -R${range} -I1 -Gv.grd 

    grdvector u.grd v.grd -JX10c -R -Gblack -S40c -Bf10 -Q0p/0.1c/0.03c -I1 -P -O -K  >> vec.ps

    psxy ${PDATA} -R -JX -O -K -Sc0.84c -W255/0/0 >> vec.ps
    psxy ${PDATA} -R -JX -O -K -Sc0.1c -G255/0/0 >> vec.ps 

    psscale -D10.5c/5c/8c/0.2c -Cvel.cpt -B5:VELOCITY:/:cm/s: -O -K >> vec.ps

    psbasemap -JS0/90/70/10c -R${MPRANGE}r -Bg0.5a2WSne -P -O >> vec.ps
#    psxy ${P2DATA} -R -JS -O -Sc0.1c -G255/0/0 -P >> vec.ps

    psconvert -A -P -Tj vec.ps

    mv vec.jpg vec_${ifile}.jpg

    rm vec.txt
    rm vel.txt
    rm cccc.txt
    rm cccc.dat
    rm u.txt
    rm v.txt
    rm *.ps

    mv vec_${ifile}.jpg vec
    
done
