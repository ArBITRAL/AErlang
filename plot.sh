#!/usr/local/bin/gnuplot

set xtic auto
set ytic auto

set title "Performance of Cache Replacement Policies"
set xlabel "Cache Size (# of processes)"
set ylabel "Time (ms)"

set terminal postscript enhanced mono dashed lw 1 'Helvetica' 11
set output 'out.eps'
set style data linespoints
plot 'AbC-LFU-Cost.txt' using 1:2 title "AbC-P1" with linespoints, \
	'AbC-LFU-Cost.txt' using 1:3 title "AbC-P2" with linespoints
