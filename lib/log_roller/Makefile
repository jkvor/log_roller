VERSION=0.4
PKG_NAME=log_roller
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`

all: rel

compile: app
	(cd src;$(MAKE))

app:
	sh ebin/$(PKG_NAME).app.in $(VERSION)	

rel: compile
	mkdir -p bin
	erl -pa ebin -eval '$(PKG_NAME):build_rel("$(VERSION)")' -s init stop -noshell

clean:
	rm -rf ebin/*.beam ebin/*.app doc/*.html doc/*.png doc/*-info doc/*.css bin/*.boot bin/*.rel bin/*.script Mnesia* erl_crash.dump *.tgz *.1 *.idx *.siz tmp

install:
	mkdir -p $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/{ebin,include}
	mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/* ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/log_roller-$(VERSION)/$$i ; done
	cp bin/*.boot $(prefix)/$(ROOTDIR)/bin/