all: compile

compile:
	(cd lib/log_roller;$(MAKE))
	(cd lib/log_roller_server;$(MAKE))
	(cd lib/log_roller_web;$(MAKE))		

clean:
	(cd lib/log_roller;$(MAKE) clean)
	(cd lib/log_roller_server;$(MAKE) clean)
	(cd lib/log_roller_web;$(MAKE) clean)		
