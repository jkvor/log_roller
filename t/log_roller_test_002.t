#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

main(_) ->
    etap:plan(5),
	ok = error_logger:tty(false),

	etap_exception:lives_ok(fun() ->
		etap_application:load_ok(log_roller_subscriber, "Application 'log_roller_subscriber' loaded"),
		etap_application:load_ok(log_roller_publisher, "Application 'log_roller_publisher' loaded"),
		ok
	end, "load log roller"),
	
	Log_Dir = rnd_dir(),
	application:set_env(log_roller_subscriber, log_dir, Log_Dir),
	application:set_env(log_roller_subscriber, maxbytes, 10485760),
	application:set_env(log_roller_subscriber, maxfiles, 10),
	
	etap_exception:lives_ok(fun() ->
		etap_application:start_ok(log_roller_subscriber, "Application 'log_roller_subscriber' started"),
		etap_application:start_ok(log_roller_publisher, "Application 'log_roller_publisher' started"),
		ok
	end, "start log roller"),
	
	Num = 10000,
	Text = "Quisque non metus at justo gravida gravida. Vivamus ullamcorper eros sed dui. In ultrices dui vel leo. Duis nisi massa, vestibulum sed, mattis quis, mollis sit amet, urna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer velit nunc, ultrices vitae, sagittis sit amet, euismod in, leo. Sed bibendum, ipsum at faucibus vulputate, est ipsum mollis odio, quis iaculis justo purus non nisl. Aenean tellus nisl, pellentesque in, consectetur non, vestibulum sit amet, nibh. Donec diam. Quisque eros. Etiam dictum tellus et ante. Donec fermentum lectus non augue. Maecenas justo. Aenean et metus ac nunc pharetra congue. Mauris rhoncus justo vitae tortor. Sed ornare tristique neque. In eu enim auctor sem tincidunt vestibulum. Aliquam erat volutpat. Nulla et diam ac magna porttitor molestie. Vestibulum massa erat, tristique sed, venenatis et, sagittis in, mauris.",
	io:format("sending logs~n"),
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],
	%% NOTE: since info messages are being sent to the log_roller handler asynchronously
	%% and then cached in the disk_log gen_server we must both wait for the disk_log to 
	%% receive them all and then flush the disk_log cache before trying to read from disk
	io:format("waiting for write to disk~n"),
	timer:sleep(5000),
	ok = log_roller_disk_logger:sync(),
	
	etap_exception:lives_ok(fun() ->
		io:format("fetching~n"),
		Res = fprof:apply(lrb, fetch, [[{max, Num}]]),
		fprof:profile(),
		fprof:analyse(),
		etap:is(length(Res), Num, "fetched correct number of results"),
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