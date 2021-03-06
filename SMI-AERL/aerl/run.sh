#!/bin/bash

erl -pa ebin/ worker_pool/ebin -s asmp run -run init stop -noshell > t1.text
for (( i=1; i <= 49; i++ ))
do
    erl -pa ebin/ worker_pool/ebin -s asmp run -run init stop -noshell >> t1.text
done

grep -E '^(Computation)' t1.text | cut -d'=' -f2 > time1
awk '{ total += $1; count++ } END { print total/count }' time1 >> avg_time1
