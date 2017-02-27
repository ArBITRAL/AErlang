#!/bin/bash
rebar compile
erl -pa psmp/ebin aerl/ebin -eval "application:start(psmp)" -run init stop -noshell > $1
./getresult.sh $1 >> log$2
