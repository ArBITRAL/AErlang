ERL=erl -I -pa ebin -noshell -eval
APPFILE=aerl.app

all: 	ebin/$(APPFILE)
	$(ERL) 'leex:file("src/aerl_scanner"), halt().'
	$(ERL) 'yecc:file("src/aerl_parser"), halt().'
	$(ERL) -make

test:	ebin/
	@erl -noshell -pa ebin -s test test

ebin/$(APPFILE):	src/$(APPFILE).src
	cp $< $@

clean:
	rm -f ebin/*.beam ebin/erl_crash.dump
	rm -f src/aerl_scanner.erl src/aerl_parser.erl
