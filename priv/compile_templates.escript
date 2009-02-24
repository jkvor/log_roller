#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
	{ok, Filenames} = file:list_dir("templates"),
	[erltl:compile("templates/" ++ Filename, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars]) 
		|| Filename <- Filenames],
	ok.
