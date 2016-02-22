#!/usr/local/bin/gnuplot

set xtic auto
set ytic auto

#set title "Performance of Cache Replacement Policies"
set xlabel "Cache Size (# of processes)"
set ylabel "Time (ms)"

set yrange[*:5000]

set terminal postscript enhanced mono dashed lw 1 'Helvetica' 11
set output 'out3policies.eps'
set style data linespoints

#set key horiz
#set key right

plot 'cap1cap2c.txt' using 1:5 title "C-erl (10%)" with linespoints lw 2 lc 5, \
	'cap1cap2c.txt' using 1:4 title "C-erl (1%)" with linespoints lw 2 lc 4, \
	'cap1cap2c.txt' using 1:2 title "C-aerl-P1 (10%)" with linespoints lw 2 lc 3, \
	'cap1cap2c.txt' using 1:3 title "C-aerl-P2 (10%)" with linespoints lw 2 lc 1
