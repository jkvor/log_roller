#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a large log set that wraps, but does not overlap on itself
%% * Test fetching correct number of logs with {max, none} and {max, Num}
%% =============================================================================
main(_) ->
    etap:plan(11),
	ok = log_roller_test:setup_test(9000000, 10),
	%stop_watch:start_link(),
	
	%% log an error that will differ in type from the info messages below
	error_logger:error_msg("Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy"),
	
	Num = 10000,
	Text = "Quisque non metus at justo gravida gravida. Vivamus ullamcorper eros sed dui. In ultrices dui vel leo. Duis nisi massa, vestibulum sed, mattis quis, mollis sit amet, urna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer velit nunc, ultrices vitae, sagittis sit amet, euismod in, leo. Sed bibendum, ipsum at faucibus vulputate, est ipsum mollis odio, quis iaculis justo purus non nisl. Aenean tellus nisl, pellentesque in, consectetur non, vestibulum sit amet, nibh. Donec diam. Quisque eros. Etiam dictum tellus et ante. Donec fermentum lectus non augue. Maecenas justo. Aenean et metus ac nunc pharetra congue. Mauris rhoncus justo vitae tortor. Sed ornare tristique neque. In eu enim auctor sem tincidunt vestibulum. Aliquam erat volutpat. Nulla et diam ac magna porttitor molestie. Vestibulum massa erat, tristique sed, venenatis et, sagittis in, mauris.",

	io:format("sending logs~n"),
	[error_logger:info_msg("~s: ~w~n", [Text, I]) || I <- lists:seq(1,Num)],

	%% NOTE: since info messages are being sent to the log_roller handler asynchronously
	%% and then cached in the disk_log gen_server we must both wait for the disk_log to 
	%% receive them all and then flush the disk_log cache before trying to read from disk
	io:format("waiting for write to disk~n"),
	log_roller_test:wait_for_queue_to_empty(),
	ok = log_roller_disk_logger:sync(default),
	
	io:format("fetching~n"),
	etap_exception:lives_ok(fun() ->
		etap:is(length(lrb:fetch(default, [{max, none}])) >= Num+1, true, "fetched correct number of results with {max, none}"),
		etap:is(length(lrb:fetch(default, [{max, Num+1}])), Num+1, "fetched correct number of results with {max, Val}"),
		etap:is(length(lrb:fetch(default, [{types, [error]}])), 1, "fetched correct number of results with {type, error}"),
		ok
	end, "fetch log"),
	
	ok = log_roller_test:teardown_test(),
	
	%stop_watch:print(),
	
    etap:end_tests().
