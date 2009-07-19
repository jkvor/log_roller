%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(log_roller_config).
-author('jacob.vorreuter@gmail.com').

-export([get_disk_loggers/0]).

-include("log_roller.hrl").

get_disk_loggers() ->
	case delete_non_disk_loggers(application:get_all_env(log_roller_server)) of
		[] ->
			[#disk_logger{}];
		Envs ->
			get_disk_loggers(Envs, [])
	end.

delete_non_disk_loggers(Envs) ->
    lists:foldl(
        fun(Key, Acc) ->
            proplists:delete(Key, Acc)
        end, Envs, [included_applications, address, port, doc_root]).

get_disk_loggers([], Loggers) -> Loggers;

get_disk_loggers([{log_dir, LogDir}|Tail], Loggers) when is_list(LogDir) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{log_dir=LogDir}|Loggers1],
	get_disk_loggers(Tail, Loggers2);

get_disk_loggers([{filters, Filters}|Tail], Loggers) when is_list(Filters) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{filters=Filters}|Loggers1],
	get_disk_loggers(Tail, Loggers2);

get_disk_loggers([{cache_size, CacheSize}|Tail], Loggers) when is_integer(CacheSize) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{cache_size=CacheSize}|Loggers1],
	get_disk_loggers(Tail, Loggers2);

get_disk_loggers([{maxbytes, Maxbytes}|Tail], Loggers) when is_integer(Maxbytes) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{maxbytes=Maxbytes}|Loggers1],
	get_disk_loggers(Tail, Loggers2);

get_disk_loggers([{maxfiles, Maxfiles}|Tail], Loggers) when is_integer(Maxfiles) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{maxfiles=Maxfiles}|Loggers1],
	get_disk_loggers(Tail, Loggers2);

get_disk_loggers([{Custom, Args}|Tail], Loggers) when is_atom(Custom), is_list(Args) ->	
	Fields = record_info(fields, disk_logger),
	Index = [I+1 || I <- lists:seq(1, length(Fields))],
	Ref = lists:zip(Fields, Index),
	Logger = populate_disk_logger_fields(#disk_logger{name=Custom}, [filters, log_dir, cache_size, maxbytes, maxfiles], Args, Ref),
	get_disk_loggers(Tail, [Logger|Loggers]).

get_default_logger(Loggers) ->
	case lists:keytake(default, 2, Loggers) of
		{value, Default, Loggers1} ->
			{Default, Loggers1};
		false ->
			{#disk_logger{}, Loggers}
	end.

populate_disk_logger_fields(Logger, [], _, _) -> Logger;

populate_disk_logger_fields(Logger, [Field|Tail], Args, Ref) ->
	case proplists:get_value(Field, Args) of
		undefined ->
			populate_disk_logger_fields(Logger, Tail, Args, Ref);
		Value ->
			Index = proplists:get_value(Field, Ref),
			Logger1 = erlang:setelement(Index, Logger, Value),
			populate_disk_logger_fields(Logger1, Tail, Args, Ref)
	end.