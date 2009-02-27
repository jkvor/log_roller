#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -config priv/log_roller -noshell

main(_) ->
    etap:plan(5),

	Log_Dir = rnd_dir(),
	ok = file:make_dir(Log_Dir),
	{ok, Log} = disk_log:open([{name, log_roller}, {file, Log_Dir ++ "/log_roller_test"}, {type, wrap}, {size, {1024, 3}}]),
	
	ok = disk_log:log(Log, {log_entry, erlang:now(), info, 'asdf@MyDogJesusMac.local', [application, start]}),
	
	Res = read_log(Log, start, []),
	io:format("results: ~p~n", [Res]),

	ok = rm_dir(Log_Dir),
	
    etap:end_tests().

read_log(Log, Continuation, Acc) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			Acc;
		{error, Reason} ->
			io:format("error: ~p~n", [Reason]),
			Acc;
		{Continuation2, Terms} ->
			read_log(Log, Continuation2, lists:append(Terms, Acc));
		{Continuation2, Terms, _Badbytes} ->
			read_log(Log, Continuation2, lists:append(Terms, Acc))
	end.
	
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