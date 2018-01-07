#!/bin/bash
msg="$(./getmsg.sh $1)"
size="$(./getsize.sh $1)"
stable="$(./getstable.sh $1)"
time="$(./gettime.sh $1)"
echo $1 "$msg" "$size" "$stable" "$time"
