#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

main(_) ->
    etap:plan(5),

	etap_exception:lives_ok(fun() ->
		etap_application:load_ok(log_roller_subscriber, "Application 'log_roller_subscriber' loaded"),
		etap_application:load_ok(log_roller_publisher, "Application 'log_roller_publisher' loaded"),
		ok
	end, "load log roller"),
	
	Log_Dir = rnd_dir(),
	application:set_env(log_roller_subscriber, log_dir, Log_Dir),
	
	etap_exception:lives_ok(fun() ->
		etap_application:start_ok(log_roller_subscriber, "Application 'log_roller_subscriber' started"),
		etap_application:start_ok(log_roller_publisher, "Application 'log_roller_publisher' started"),
		ok
	end, "start log roller"),
	
	ok = error_logger:info_msg("this is a test~n"),
	
	etap_exception:lives_ok(fun() ->
		{ok, Binary} = file:read_file(Log_Dir ++ "/log_roller_data.1"),
		io:format("bin: ~p~n", [Binary]),
		ok
	end, "read file"),
	
	etap_exception:lives_ok(fun() ->
		io:format("~p~n", [lrb:fetch()]),
		ok
	end, "fetch log"),
	
	etap:is(rm_dir(Log_Dir), ok, "remove temp log directory"),

    etap:end_tests().

rnd_dir() ->
	{A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
	Dir = "." ++ lists:flatten([
        [[random:uniform(25) + 96] || _ <-lists:seq(1,5)],
        [[random:uniform(9) + 47] || _ <-lists:seq(1,3)]
    ]),
	io:format("temp log directory: ~p~n", [Dir]),
	Dir.

rm_dir(Dir) ->
	{ok, Filenames} = file:list_dir(Dir),
	[file:delete(Dir ++ "/" ++ File) || File <- Filenames],
	file:del_dir(Dir).