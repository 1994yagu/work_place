#!bin/sh

range=1/42/1/42
MPRANGE=-158.973068/74.8304062/-167.494202/73.5827866
PDATA=moor_point.txt
P2DATA=moor_latlon.txt
list1=cul_file1.txt
list2=cul_file2.txt
MV=moor_vel.txt

RDIR=/Users/yaguchi/HDD2/northpole/moor/201408_09/out/ref

count=1

#makecpt -Cgray -T-500/11000/500 -Z > ref.cpt

while read 

do
    #make reflectance grid
    
    ref1=`awk 'NR==cc' cc=${count} cul_file1.txt `

    ref1=`echo ${ref1} | cut -c 1-16`

    cp ${RDIR}/${ref1} ./ref1.dat

#    echo $RDIR/$ref1

    ./convert_ref-o

    xyz2grd ref1.txt -R1/840/1/840 -I1 -Gref1.grd

    max=0
#    echo $count
    max1=`cat ref1.txt | awk '{if(max<$3) max=$3} END{print max}'`
#    echo "max="$max
    min1=`cat ref1.txt | awk 'BEGIN{min=1000000} {if($3 != "" && min>$3) min=$3} END{print min}'`
    #    echo "min="$min


    makecpt -Cgray -T$min1/$max1/100 -Z > ref1.cpt


    #make vector grid

    vec1=`echo ${ref1} | cut -c 1-12`
    vec2=`echo ${ref2} | cut -c 1-12`
    f=${vec1}_20.dat
    cp ${f} cccc.dat

    ./mkvec

    awk '{if($3 < 999){print $1,$2,$3} else print $1,$2,"NaN"}' cccc.txt > u.txt
    awk '{if($4 < 999){print $1,$2,$4} else print $1,$2,"NaN"}' cccc.txt > v.txt

    xyz2grd u.txt -R${range} -I1 -Gu.grd 
    xyz2grd v.txt -R${range} -I1 -Gv.grd

    

    #drow ref
    
    grdimage ref1.grd -JX10c/10c -Q -Cref1.cpt -K -P > ref1.ps
    grdimage ref2.grd -JX10c/10c -Q -Cref2.cpt -K -P > ref2.ps
  

    #drow vector

    grdvector u.grd v.grd -JX10c -R -Gblack -S40c -Bf10 -Q0p/0.1c/0.03c -I1 -G0/255/0 -P -O -K  >> ref1.ps
    grdvector u.grd v.grd -JX10c -R -Gblack -S40c -Bf10 -Q0p/0.1c/0.03c -I1 -G0/255/0 -P -O -K  >> ref2.ps

    psxy ${PDATA} -R -JX -O -K -Sc0.84c -W255/0/0 >> ref1.ps
    psxy ${PDATA} -R -JX -O -K -Sc0.84c -W255/0/0 >> ref2.ps
    psxy ${PDATA} -R -JX -O -K -Sc0.1c -G255/0/0 >> ref1.ps
    psxy ${PDATA} -R -JX -O -K -Sc0.1c -G255/0/0 >> ref2.ps

    awk 'NR==cc' cc=${count} moor_vel.txt > mv.txt
    psxy mv.txt -Sv0.1c+ea -Gred -R -JX -O -K >> ref1.ps
    psxy mv.txt -Sv0.1c+ea -Gred -R -JX -O -K >> ref2.ps

  

    psbasemap -JS0/90/70/10c -R${MPRANGE}r -Bg0.5a2WSne -P -O >> ref1.ps
    psbasemap -JS0/90/70/10c -R${MPRANGE}r -Bg0.5a2WSne -P -O >> ref2.ps
    #    psxy ${P2DATA} -R -JS -O -Sc0.1c -G255/0/0 -P >> vec.ps

    

    psconvert -A -P -Tj ref1.ps
    psconvert -A -P -Tj ref2.ps

    mv ref1.jpg ref/${vec1}_1.jpg
    mv ref2.jpg ref/${vec1}_2.jpg

    rm ref*.txt
    rm ref*.dat
    rm cccc.txt
    rm cccc.dat
    rm mv.txt
    rm u.txt
    rm v.txt
    rm *.ps
    rm *.cpt

#    if [ $count = 2 ]; then
#	exit
#    fi
    
    count=` expr $count + 1 `
    
done < cul_file1.txt


