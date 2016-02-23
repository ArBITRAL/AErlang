# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -I ./include -pa ebin -o ebin $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# Edit the lines below
MODS = aerlang aerl_trans

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: compile

compile: ${MODS:%=%.beam} 

## run an application from the makefile
#run: compile
#	echo "${ARGS}"
#	${ERL} -s test start 1000

# remove all the code
clean:	
	rm -rf *.beam erl_crash.dump
