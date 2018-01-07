#!/bin/bash
rebar compile
erl -pa asmp/ebin aerl/ebin -eval "application:start(asmp)" -run init stop -noshell > $1
./getresult.sh $1 >> log$2
