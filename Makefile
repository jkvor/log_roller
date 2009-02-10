SASL_DIR=`erl -eval 'io:format("~s~n", [code:lib_dir("sasl")])' -s init stop -noshell`

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

templates: all
	erl -pa ebin -eval 'log_roller:compile_templates()' -s init stop -noshell

clean:
	rm -rf ebin/*.beam *.boot *.rel *.script Mnesia* erl_crash.dump

install: templates
	install ebin/* $(SASL_DIR)/ebin

rel: templates
	erl -pa ebin -noshell -run log_roller build_rel -s init stop

./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<