#!/usr/bin/gnuplot

set xtic auto
set ytic auto

set title "Performance Comparison"
set xlabel "# of threads"
set ylabel "Execution time (s)"
#set yrange [1:]
#set xrange [1:12]
#set logscale x 
#set logscale y
#set logscale y 2 
#set logscale x 2 
set xrange [1:24]
set yrange [0:]
set xtics 1,2,10

set terminal postscript enhanced mono dashed lw 1 'Helvetica' 11
set output 'erl-pworkload.eps'
set style data linespoints

plot 'results.pqsort.1000' using ($1):($2/1000) title "Erlang-pqsort-1000" with linespoints, \
 'results.pqsort.5000' using ($1):($2/1000) title "Erlang-pqsort-5000" with linespoints, \
 'results.pqsort.10000' using ($1):($2/1000) title "Erlang-pqsort-10000" with linespoints
# 'results.count.10000' using ($1):($2/1000) title "Erlang-counter-10000" with linespoints, \
# 'results.count.100000' using ($1):($2/1000) title "Erlang-counter-100000" with linespoints, \
# 'results.count.1000000' using ($1):($2/1000) title "Erlang-counter-1000000" with linespoints

