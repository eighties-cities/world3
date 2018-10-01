#!/bin/bash

echo "
set terminal png size 1280,640
set output '$1'
set datafile separator \",\"
set title \"World3\"
set xlabel \"Year\"
#set size ratio 0.5
#set size 0.5,0.5
set key outside
plot \
'/tmp/results.csv' using 1:2 title columnhead(2) w l lw 2 lc rgbcolor \"#e07154\", \
'/tmp/results.csv' using 1:3 title columnhead(3) w l lw 2 lc rgbcolor \"#b0875e\", \
'/tmp/results.csv' using 1:4 title columnhead(4) w l lw 2 lc rgbcolor \"#4a8a91\", \
'/tmp/results.csv' using 1:5 title columnhead(5) w l lw 2 lc rgbcolor \"#a8c3a5\", \
'/tmp/results.csv' using 1:6 title columnhead(6) w l lw 2 lc rgbcolor \"#4a6892\", \
'/tmp/results.csv' using 1:7 title columnhead(7) w l lw 2 lc rgbcolor \"#a25563\", \
'/tmp/results.csv' using 1:8 title columnhead(8) w l lw 2 lc rgbcolor \"#666666\", \
'/tmp/results.csv' using 1:9 title columnhead(9) w l lw 2 lc rgbcolor \"#f6f648\", \
'/tmp/results.csv' using 1:10 title columnhead(10) w l lw 2 lc rgbcolor \"#650d99\", \
'/tmp/results.csv' using 1:11 title columnhead(11) w l lw 2 lc rgbcolor \"#513210\", \
'/tmp/results.csv' using 1:12 title columnhead(12) w l lw 2 lc rgbcolor \"#185103\"
" | gnuplot

