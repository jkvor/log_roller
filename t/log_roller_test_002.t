#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% =============================================================================
main(_) ->
    etap:plan(1),

	log_roller_test:load_apps(),
	put(log_dir, log_roller_test:rnd_dir()),
	application:set_env(log_roller_server, log_dir, get(log_dir)),
	application:set_env(log_roller_server, cache_size, 90000),
	application:set_env(log_roller_server, maxbytes, 9000000),
	application:set_env(log_roller_server, maxfiles, 10),
	log_roller_test:start_apps(),

    etap:end_tests().