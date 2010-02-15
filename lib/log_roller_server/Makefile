VERSION=0.4
PKG_NAME=log_roller_server
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`

all: rel

compile: app
	(cd src;$(MAKE))

app: 
	sh ebin/$(PKG_NAME).app.in $(VERSION)

rel: templates
	mkdir -p bin
	erl -pa ebin ../log_roller/ebin -eval '$(PKG_NAME):build_rel("$(VERSION)")' -s init stop -noshell

templates: compile
	erl -pa ebin -eval '$(PKG_NAME):compile_templates()' -s init stop -noshell	
	
clean:
	rm -rf ebin/*.beam ebin/*.app doc/*.html doc/*.png doc/*-info doc/*.css bin/*.boot bin/*.rel bin/*.script Mnesia* erl_crash.dump *.tgz *.1 *.idx *.siz tmp

install:
	mkdir -p $(prefix)/$(LIBDIR)/log_roller_server-$(VERSION)/{ebin,public}	
	mkdir -p $(prefix)/$(ROOTDIR)/bin
	cp -r public/* $(prefix)/$(LIBDIR)/log_roller_server-$(VERSION)/public/	
	for i in ebin/*.beam ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/log_roller_server-$(VERSION)/$$i ; done
	cp bin/*.boot $(prefix)/$(ROOTDIR)/bin/
	@mkdir -p $(prefix)/etc/init.d
	cp log_roller $(prefix)/etc/init.d/