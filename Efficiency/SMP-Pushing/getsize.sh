#!/bin/bash
grep -E '^(Matching Size)' $1 | cut -d'=' -f2 > time1
awk '{ total += $1; count++ } END { print total/count }' time1
rm time1

