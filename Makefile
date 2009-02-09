SASL_DIR=`erl -eval 'io:format("~s~n", [code:lib_dir("sasl")])' -s init stop -noshell`

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: $(ERL_OBJECTS)

clean:
	rm -rf ebin/*.beam *.boot *.rel *.script Mnesia* crash_report.dump

install: all
	install ebin/* $(SASL_DIR)/ebin

rel: all
	erl -pa ebin -noshell -run log_roller build_rel -s init stop

./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -o ebin $<