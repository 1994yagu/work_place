#!/bin/sh


gmtset FONT_ANNOT_PRIMARY 8p,,
gmtset FONT_LABEL 10p,,
gmtset FONT_LOGO 10p,,
gmtset FONT_TITLE 20p,,

win_size=10
ifile=all_compare.txt
#ifile2=no_ice.txt
#ifile3=icon_dif.txt
ofile1=scatter_u_all
ofile2=scatter_v_all
#ofile3=sccater_icon_x
#ofile4=sccater_icon_y
xlabel=calcurate_velocity
ylabel=moor_velocity

CPT=icon.cpt

#cat compare1.txt compare2.txt compare3.txt > compare.txt
#cp compare1.txt compare.txt

awk 'BEGIN{for(x=-60;x<=60;x+=1) print x, x}' > x.txt

awk '{print $1,$2}' ${ifile} > temp_x.txt
#awk '{print $1,$2}' ${ifile2} > temp_x2.txt
awk '{print $3,$4}' ${ifile} > temp_y.txt
#awk '{print $3,$4}' ${ifile2} > temp_y2.txt
#awk '{print $1,$2,$5}' ${ifile3} > temp_x3.txt
#awk '{print $3,$4,$5}' ${ifile3} > temp_y3.txt
#awk '{print $5,$6}' ${ifile} > temp_s.txt


psbasemap -R-50/50/-50/50 -JX10/10 -Ba10:"calculate velocity[cm/s]":/a10:"moor velocity[cm/s]":WeSn:.x_direction: -P -K  > ${ofile1}.ps
psxy temp_x.txt -R -JX -Gblue -Wblue -Sc0.1 -P -O -K  >> ${ofile1}.ps
#psxy temp_x2.txt -R -JX -Gred -Wred -Sc0.05 -P -O -K  >> ${ofile1}.ps
#pslegend -J -R -V -P -O -Dx2/8/2c/4c  << EOF >> ${ofile1}.ps
#N 1
#S 1c 0.05c 0/0/255 0.5p 1c Ice 
#S 1c 0.05c 255/0/0 0.5p 1c NO_Ice
#EOF
psxy x.txt -R -JX -P -O  >> ${ofile1}.ps

psbasemap -R-50/50/-50/50 -JX10/10 -Ba10:"calculate velocity[cm/s]":/a10:"moor velocity[cm/s]":WeSn:.y_direction: -P -K  > ${ofile2}.ps
psxy temp_y.txt -R -JX -Gblue -Wblue -Sc0.1 -P -O -K >> ${ofile2}.ps
#psxy temp_y2.txt -R -JX -Gred -Wred -Sc0.05 -P -O -K >> ${ofile2}.ps
psxy x.txt -R -JX -P -O  >> ${ofile2}.ps

makecpt -Cno_green -T0/1/0.01 > ${CPT}

#psbasemap -R-50/50/-50/50 -JX10/10 -Ba10:"calculate velocity[cm/s]":/a10:"moor velocity[cm/s]":WeSn:.x_direction_August: -P -K  > ${ofile3}.ps
#psxy temp_x3.txt -R -JX -C${CPT} -Sc0.1 -P -O -K >> ${ofile3}.ps
#psscale -D10.5/4/8/0.4 -B0.1:"ice concentration": -C${CPT} -O -K >> ${ofile3}.ps
#psxy x.txt -R -JX -P -O  >> ${ofile3}.ps

#psbasemap -R-50/50/-50/50 -JX10/10 -Ba10:"calculate velocity[cm/s]":/a10:"moor velocity[cm/s]":WeSn:.y_direction_August: -P -K  > ${ofile4}.ps
#psxy temp_y3.txt -R -JX -C${CPT} -Sc0.1 -P -O -K >> ${ofile4}.ps
#psscale -D10.5/4/8/0.4 -B0.1:"ice concentration": -C${CPT} -O -K >> ${ofile4}.ps
#psxy x.txt -R -JX -P -O  >> ${ofile4}.ps

#psbasemap -R-60/60/-60/60 -JX10/10 -Ba10:"calculate angle[rad]":/a10:"moor angle[rad]":WeSn:.angle: -P -K  > ${ofile3}.ps
#psxy temp_s.txt -R -JX -Gred -Wred -Sc0.05 -P -O -K >> ${ofile3}.ps
#psxy x.txt -R -JX -P -O  >> ${ofile3}.ps

#convert -density 300 ${ofile1}.ps temp1.jpg
#convert -density 300 ${ofile2}.ps temp2.jpg
#convert -density 300 ${ofile3}.ps temp3.jpg

#convert -trim -border 5%x5% -bordercolor white temp1.jpg ${ofile1}.jpg
#convert -trim -border 5%x5% -bordercolor white temp2.jpg ${ofile2}.jpg
#convert -trim -border 5%x5% -bordercolor white temp3.jpg ${ofile3}.jpg

psconvert -A -P -Tj ${ofile1}.ps
psconvert -A -P -Tj ${ofile2}.ps
#psconvert -A -P -Tj ${ofile3}.ps
#psconvert -A -P -Tj ${ofile4}.ps


rm ${ofile1}.ps
rm ${ofile2}.ps
#rm ${ofile3}.ps
#rm ${ofile4}.ps

#rm temp*.txt
#rm temp*.jpg
