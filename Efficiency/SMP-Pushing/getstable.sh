#!/bin/bash
total=`grep -E '^(Stable)' $1 | wc -l` 
item=`grep -E '^(Stable=true)' $1 | wc -l`
percent=$(awk "BEGIN { pc=100*${item}/${total}; i=int(pc); print (pc-i<0.5)?i:i+1 }")
echo $percent
