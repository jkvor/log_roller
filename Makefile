VERSION=0.2
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

templates: all
	erl -pa ebin -eval 'log_roller_server:compile_templates()' -s init stop -noshell

clean:
	rm -rf ebin/*.beam doc/*.html doc/*.png doc/*-info doc/*.css bin/*.boot bin/*.rel bin/*.script Mnesia* erl_crash.dump *.tgz *.1 *.idx *.siz

docs: all
	erl -eval 'edoc:application(log_roller, ".", [])' -s init stop -noinput

install: rel
	mkdir -p $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/{ebin,include,priv}
	mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/* priv/* ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/$$i ; done
	cp bin/*.boot $(prefix)/$(ROOTDIR)/bin/
	@mkdir -p $(prefix)/etc/init.d
	mkdir -p $(prefix)/var/log/log_roller
	cp log_roller $(prefix)/etc/init.d/

uninstall:
	rm -rf $(LIBDIR)/log_roller*
	rm $(ROOTDIR)/bin/log_roller*
	rm /etc/init.d/log_roller*

package: clean
	@mkdir log_roller-$(VERSION)/ && cp -rf bin ebin include log_roller Makefile priv README src t templates log_roller-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf log_roller-$(VERSION).tgz log_roller-$(VERSION)
	@rm -rf log_roller-$(VERSION)/

rel: templates
	mkdir -p bin
	erl -pa ebin -eval 'log_roller_server:build_rel()' -s init stop -noshell

test:
	prove t/*.t

./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<