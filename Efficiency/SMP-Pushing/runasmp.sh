#!/bin/bash
make
erl -pa aerl/ebin asmp/ebin tinymq/ebin tinymq/deps/tiny_pq/ebin -eval "application:start(asmp)" -run init stop -noshell
