#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -noshell

%% =============================================================================
%% * Test a wrapping and overwriting log
%% =============================================================================
main(_) ->
	Num = 100,
	Text = "Quisque non metus at justo gravida gravida ",

    etap:plan(Num),
	
	Cache = log_roller_cache:start(102400),
		
	[log_roller_cache:put(Cache, integer_to_list(I), list_to_binary(Text ++ integer_to_list(I))) || I <- lists:seq(1,Num)],
	
	[begin
		etap:is(binary_to_list(log_roller_cache:get(Cache, integer_to_list(I))), "Quisque non metus at justo gravida gravida " ++ integer_to_list(I), "cache value matches")
	 end || I <- lists:seq(1, Num)],
	
    etap:end_tests().
	
	