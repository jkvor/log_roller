VERSION=0.0.1
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`
SASL_DIR=`erl -eval 'io:format("~s~n", [code:lib_dir("sasl")])' -s init stop -noshell`

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

templates: all
	escript priv/compile_templates.escript

clean:
	rm -rf ebin/*.beam doc/*.html doc/*.png doc/*-info doc/*.css bin/*.boot bin/*.rel bin/*.script Mnesia* erl_crash.dump *.tgz *.1 *.idx *.siz
	
docs: all
	erl -eval 'edoc:application(log_roller, ".", [])' -s init stop -noinput

install: rel
	mkdir -p ${LIBDIR}/log_roller-$(VERSION)/{ebin,include,priv}
	for i in ebin/*.beam include/* priv/* ebin/*.app; do install $$i ${LIBDIR}/log_roller-$(VERSION)/$$i ; done
	cp bin/*.boot $(ROOTDIR)/bin/
	mkdir -p /etc/init.d
	mkdir -p /var/log/log_roller
	cp log_roller_subscriber /etc/init.d/
	
uninstall:
	rm -rf ${LIBDIR}/log_roller-*
	rm $(ROOTDIR)/bin/log_roller*.boot
	rm /etc/init.d/log_roller*

package: clean
	@mkdir log_roller-$(VERSION)/ && cp -rf ebin include log_roller Makefile priv README src templates log_roller-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf log_roller-$(VERSION).tgz log_roller-$(VERSION)
	@rm -rf log_roller-$(VERSION)/
		
rel: templates
	mkdir -p bin
	escript priv/build_rel.escript subscriber
	escript priv/build_rel.escript publisher

test: all
	prove -v t/*.t
	
./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<