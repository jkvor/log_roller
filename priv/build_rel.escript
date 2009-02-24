#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main(["subscriber"]) ->
	{ok, FD} = file:open("bin/log_roller_subscriber.rel", [write]),
	RootDir = code:root_dir(),
	Patterns = [
	    {RootDir ++ "/", "erts-*"},
	    {RootDir ++ "/lib/", "kernel-*"},
	    {RootDir ++ "/lib/", "stdlib-*"}
	],
	[Erts, Kerne, Stdl] = [begin
	    [R | _ ] = filelib:wildcard(P, D),
	    [_ | [Ra] ] = string:tokens(R, "-"),
	    Ra
	end || {D, P} <- Patterns],
	RelInfo = {release,
	    {"log_roller_subscriber", "0.0.1"},
	    {erts, Erts}, [
	        {kernel, Kerne},
	        {stdlib, Stdl},
	        {log_roller_subscriber, "0.0.1"}
	    ]
	},
	io:format(FD, "~p.", [RelInfo]),
	file:close(FD),
	systools:make_script("bin/log_roller_subscriber", [local]),
	ok;

main(["publisher"]) ->
	{ok, FD} = file:open("bin/log_roller_publisher.rel", [write]),
	RootDir = code:root_dir(),
	Patterns = [
	    {RootDir ++ "/", "erts-*"},
	    {RootDir ++ "/lib/", "kernel-*"},
	    {RootDir ++ "/lib/", "stdlib-*"}
	],
	[Erts, Kerne, Stdl] = [begin
	    [R | _ ] = filelib:wildcard(P, D),
	    [_ | [Ra] ] = string:tokens(R, "-"),
	    Ra
	end || {D, P} <- Patterns],
	RelInfo = {release,
	    {"log_roller_publisher", "0.0.1"},
	    {erts, Erts}, [
	        {kernel, Kerne},
	        {stdlib, Stdl},
	        {log_roller_publisher, "0.0.1"}
	    ]
	},
	io:format(FD, "~p.", [RelInfo]),
	file:close(FD),
	systools:make_script("bin/log_roller_publisher", [local]),
	ok.