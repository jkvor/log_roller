VERSION=0.4

all: compile

compile:
	(cd lib/log_roller;$(MAKE))
	(cd lib/log_roller_server;$(MAKE))
	(cd lib/log_roller_web;$(MAKE))

clean:
	(cd lib/log_roller;$(MAKE) clean)
	(cd lib/log_roller_server;$(MAKE) clean)
	(cd lib/log_roller_web;$(MAKE) clean)
	
install:
	(cd lib/log_roller;$(MAKE) install $(prefix))
	(cd lib/log_roller_server;$(MAKE) install $(prefix))
	(cd lib/log_roller_web;$(MAKE) install $(prefix))

package: clean
	@mkdir log_roller-$(VERSION)/ && cp -rf lib Makefile README.markdown log_roller-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf log_roller-$(VERSION).tgz log_roller-$(VERSION)
	@rm -rf log_roller-$(VERSION)/
