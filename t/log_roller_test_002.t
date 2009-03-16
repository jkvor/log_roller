#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a wrapping and overwriting log
%% =============================================================================
main(_) ->
    etap:plan(11),
	ok = log_roller_test:setup_test(200000, 5),
	%stop_watch:start_link(),
	
	Num = 1000,
	Text = "Quisque non metus at justo gravida gravida. Vivamus ullamcorper eros sed dui. In ultrices dui vel leo. Duis nisi massa, vestibulum sed, mattis quis, mollis sit amet, urna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer velit nunc, ultrices vitae, sagittis sit amet, euismod in, leo. Sed bibendum, ipsum at faucibus vulputate, est ipsum mollis odio, quis iaculis justo purus non nisl. Aenean tellus nisl, pellentesque in, consectetur non, vestibulum sit amet, nibh. Donec diam. Quisque eros. Etiam dictum tellus et ante. Donec fermentum lectus non augue. Maecenas justo. Aenean et metus ac nunc pharetra congue. Mauris rhoncus justo vitae tortor. Sed ornare tristique neque. In eu enim auctor sem tincidunt vestibulum. Aliquam erat volutpat. Nulla et diam ac magna porttitor molestie. Vestibulum massa erat, tristique sed, venenatis et, sagittis in, mauris.",

	io:format("sending logs~n"),
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],

	%% NOTE: since info messages are being sent to the log_roller handler asynchronously
	%% and then cached in the disk_log gen_server we must both wait for the disk_log to 
	%% receive them all and then flush the disk_log cache before trying to read from disk
	io:format("waiting for write to disk~n"),
	timer:sleep(3000),
	ok = log_roller_disk_logger:sync(),
	
	io:format("fetching~n"),
	etap_exception:lives_ok(fun() ->
		%io:format("length: ~p~n", [length(lrb:fetch([{max, Num}]))]),
		etap:is(length(lrb:fetch([{max, Num}])), Num, "fetched correct number of results"),
		etap:is(length(lrb:fetch([{max, Num}])), Num, "fetched correct number of results"),
		ok
	end, "fetch log"),
	
	io:format("sending logs~n"),
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],
	
	io:format("waiting for write to disk~n"),
	timer:sleep(3000),
	ok = log_roller_disk_logger:sync(),
	
	io:format("fetching~n"),
	etap_exception:lives_ok(fun() ->
		Res = lrb:fetch([{max, Num}]),
		io:format("terms: ~p~n", [Res]),
		etap:is(length(Res), Num, "fetched correct number of results"),
		%etap:is(length(lrb:fetch([{max, Num}])), Num, "fetched correct number of results"),
		ok
	end, "fetch log"),
	
	ok = log_roller_test:teardown_test(),
	
	%stop_watch:print(),
	
    etap:end_tests().
