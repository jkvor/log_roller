VERSION=0.0.1
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`
SASL_DIR=`erl -eval 'io:format("~s~n", [code:lib_dir("sasl")])' -s init stop -noshell`

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

templates: all
	erl -pa ebin -eval 'log_roller:compile_templates()' -s init stop -noshell

clean:
	rm -rf ebin/*.beam *.boot *.rel *.script Mnesia* erl_crash.dump *.tgz

install: rel
	mkdir -p ${LIBDIR}/log_roller-$(VERSION)/{ebin,include,priv}
	for i in ebin/*.beam include/* priv/* ebin/*.app; do install $$i ${LIBDIR}/log_roller-$(VERSION)/$$i ; done
	cp log_roller.boot $(ROOTDIR)/bin/
	mkdir -p /etc/init.d
	mkdir -p /var/log/log_roller
	cp log_roller /etc/init.d/

package: clean
	@mkdir log_roller-$(VERSION)/ && cp -rf ebin include log_roller Makefile priv README src templates log_roller-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf log_roller-$(VERSION).tgz log_roller-$(VERSION)
	@rm -rf log_roller-$(VERSION)/
		
rel: templates
	erl -pa ebin -noshell -run log_roller build_rel -s init stop

./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<