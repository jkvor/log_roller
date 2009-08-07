VERSION=0.3
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`

all: rel

compile:
	(cd src;$(MAKE))

templates: compile
	erl -pa ebin -eval 'log_roller_server:compile_templates()' -s init stop -noshell

rel: templates
	mkdir -p bin
	erl -pa ebin -eval 'log_roller_server:build_rel()' -s init stop -noshell

clean:
	rm -rf ebin/*.beam doc/*.html doc/*.png doc/*-info doc/*.css bin/*.boot bin/*.rel bin/*.script Mnesia* erl_crash.dump *.tgz *.1 *.idx *.siz

docs: all
	erl -eval 'edoc:application(log_roller, ".", [])' -s init stop -noinput

install:
	mkdir -p $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/{ebin,include,priv,public}
	mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/* priv/* ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/$$i ; done
	cp -r public/* $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/public/
	cp bin/*.boot $(prefix)/$(ROOTDIR)/bin/
	@mkdir -p $(prefix)/etc/init.d
	cp log_roller $(prefix)/etc/init.d/

package: clean
	@mkdir log_roller-$(VERSION)/ && cp -rf bin ebin include log_roller Makefile priv public README src support t templates log_roller-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf log_roller-$(VERSION).tgz log_roller-$(VERSION)
	@rm -rf log_roller-$(VERSION)/
	
test:
	prove t/*.t