.SUFFIXES: .erl .beam 

.erl.beam:
	erlc -W -pa ebin -o ebin $<

ERL = erl -boot start_clean 

MODS = src/aerlang src/aerl_trans test/test_send test/test_receive

all: compile
	${ERL} -pa ebin -s test_receive start

compile: ${MODS:%=%.beam} 

clean:	
	cd ebin; rm -rf *.beam erl_crash.dump
