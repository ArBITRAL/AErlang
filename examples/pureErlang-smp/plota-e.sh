#!/usr/bin/gnuplot

set xtic auto
set ytic auto

set title "Performance Comparison"
set xlabel "# of cores"
set ylabel "Execution time (s)"
#set yrange [1:]
#set xrange [1:12]
#set logscale x 
#set logscale y
#set logscale y 2 
#set logscale x 2 
set xrange [1:12]
set yrange [0:]
set terminal postscript enhanced mono dashed lw 1 'Helvetica' 11
set output 'aerl-erl.eps'
set style data linespoints

plot 'results.erl.csv' using 1:2 title "Erlang-flatten" with linespoints, \
 'results.aerl.csv' using 1:2 title "AErlang-flatten" with linespoints, \

