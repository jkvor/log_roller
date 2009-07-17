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
%%
%% @doc Browse log files collected by a log_roller subscriber node.
%% log roller browser is a gen_server that is started by the log_roller 
%% application when the "-log_roller_type subscriber" argument is given 
%% in the shell.
-module(lrb).
-author('jacob.vorreuter@gmail.com').

-export([start_link/1, start/2, server_loop/1]).

-export([
	set_current_file/2, 
	fetch/1, fetch/2, 
	get_continuation/1,
	get_continuation/2,
	fetch_by_continuation/1,
	fetch_by_continuation/2,
	disk_logger_names/0
]).

-include("log_roller.hrl").

-define(TIMEOUT, infinity).
-define(DEFAULT_MAX, 20).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link([disk_logger()]) -> {ok, pid()}
start_link(DiskLoggers) ->
	proc_lib:start(?MODULE, start, [self(), DiskLoggers]).
    
%% @spec set_current_file(LoggerName, Index) -> ok
%%		 LoggerName = atom()
%%		 Index = integer()
%% @doc when a log file is closed, refresh it in the cache
set_current_file(LoggerName, Index) when is_atom(LoggerName), is_integer(Index) ->
	call({invalidate_cache, LoggerName, Index}).
	
%% @equiv fetch(LoggerName::atom(), [])
fetch(LoggerName) when is_atom(LoggerName) -> fetch(LoggerName, []).

%% @spec fetch(LoggerName, Opts) -> Result
%%		 LoggerName = atom()
%% 		 Opts = [{max, integer()} |
%%				 {types, [atom()]} |
%%				 {nodes, [node()]} |
%%				 {grep, string()}]
%%		 Result = list(list(Time::string(), Type::atom(), Node::atom(), Message::any()))
%% @doc fetch a list of log entries for a specific disk_logger
fetch(LoggerName, Opts) when is_atom(LoggerName), is_list(Opts) ->
	call({fetch, LoggerName, Opts}).
	
get_continuation(LoggerName) when is_atom(LoggerName) ->
	get_continuation(LoggerName, []).
	
get_continuation(LoggerName, Opts) when is_atom(LoggerName), is_list(Opts) ->
	call({continuation, LoggerName, Opts}).
	
fetch_by_continuation(Continuation) ->
	fetch_by_continuation(Continuation, []).
	
fetch_by_continuation(Continuation, Opts) when is_tuple(Continuation), is_list(Opts) ->
	Max = proplists:get_value(max, Opts, ?DEFAULT_MAX),
	call({fetch, Opts, Max, Continuation}).

%% @spec disk_logger_names() -> [atom()]
disk_logger_names() ->
	call(disk_logger_names).
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start(Parent, DiskLoggers) ->
	State =
		[begin
			DiskLogger#disk_logger{cache=log_roller_cache:add(DiskLogger#disk_logger.cache_size)}
		 end || DiskLogger <- DiskLoggers],
    global:register_name(log_roller_browser_state, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    ?MODULE:server_loop(State).

server_loop(State) ->
	receive
        {From, state} ->
			From ! {ok, State}
    end,
    server_loop(State).

state() ->
	global:send(log_roller_browser_state, {self(), state}),
	receive
		{ok, State} -> State
	after ?TIMEOUT -> exit({state, timeout}) end.
	
call(Term) ->
	Self = self(),
	State = state(),
	Pid = spawn_link(fun() -> handle_call(Term, Self, State) end),
	receive
		{Pid, Result} -> Result
	after ?TIMEOUT -> exit({call, timeout}) end.

handle_call(Term, From, State) ->
	Self = self(),
	Result = handle_call(Term, State),
	From ! {Self, Result}.
	
handle_call({invalidate_cache, LoggerName, Index}, DiskLoggers) ->
	invalidate_cache_internal(disk_logger_by_name(LoggerName, DiskLoggers), Index);
	
handle_call({fetch, LoggerName, Opts}, DiskLoggers) ->
	fetch_internal(disk_logger_by_name(LoggerName, DiskLoggers), Opts);

handle_call({fetch, Opts, Max, Continuation}, _DiskLoggers) ->
	fetch_by_continuation(Opts, Max, [], Continuation);

handle_call({continuation, LoggerName, Opts}, DiskLoggers) ->
	DiskLogger = disk_logger_by_name(LoggerName, DiskLoggers),
	new_continuation(DiskLogger, Opts);
	
handle_call(disk_logger_names, DiskLoggers) ->
	[Name || #disk_logger{name=Name} <- DiskLoggers].
	
disk_logger_by_name(Name, Loggers) -> 
	case lists:keysearch(Name, 2, Loggers) of
		{value, DiskLogger} ->
			DiskLogger;
		false ->
			error_logger:error_report({?MODULE, fetch, disk_logger_not_found, Name}),
			exit({error, disk_logger_not_found})
	end.
	
invalidate_cache_internal(Logger, Index) ->
	log_roller_disk_reader:invalidate_cache(Logger#disk_logger.name, Logger#disk_logger.cache, Index).
	
fetch_internal(DiskLogger, Opts) ->
	Max = proplists:get_value(max, Opts, ?DEFAULT_MAX),
	{ok, Cont} = new_continuation(DiskLogger, Opts),
	fetch_internal(Opts, Max, [], Cont).
	
fetch_internal(Opts, Max, Acc, Cont) ->
	case fetch_by_continuation(Opts, Max, Acc, Cont) of
		{ok, Results, Cont1} ->
			fetch_internal(Opts, Max, Results, Cont1);
		{ok, Results} ->
			lists:reverse(Results)
	end.
	
new_continuation(DiskLogger, Opts) ->
	UseCache = proplists:get_value(cache, Opts, true),
	log_roller_disk_reader:start_continuation(DiskLogger#disk_logger.name, DiskLogger#disk_logger.cache, UseCache).
	
fetch_by_continuation(_, Max, Acc, _) when is_integer(Max), length(Acc) >= Max -> {ok, Acc};

fetch_by_continuation(Opts, Max, Acc, Continuation) ->
	case (catch log_roller_disk_reader:terms(Continuation)) of
		{ok, Continuation1, Terms} ->
			NumItems = ?GET_CSTATE(Continuation1, num_items),
			Acc1 = filter(Terms, Opts, Max, Acc),
			{ok, Acc1, ?SET_CSTATE(Continuation1, num_items, NumItems+length(Acc1))};
		{'EXIT', {error, read_full_cycle}} ->
			{ok, Acc};
		{'EXIT', Error} ->
			error_logger:error_report({?MODULE, fetch_internal, Error}),
			exit(Error)
	end.
	
filter([], _Opts, _Max, Acc) -> Acc;

filter(_Terms, _Opts, Max, Acc) when is_integer(Max), length(Acc) >= Max -> Acc;

filter([LogEntry|Tail], Opts, Max, Acc) ->
	case log_roller_filter:filter(LogEntry, Opts) of
		[] ->
			filter(Tail, Opts, Max, Acc);
		[_] ->
			Term = [format_time(LogEntry#log_entry.time), LogEntry#log_entry.type, LogEntry#log_entry.node, LogEntry#log_entry.message],
			filter(Tail, Opts, Max, [Term|Acc])
	end.
	
format_time({Mega,Secs,Micro}) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time({Mega,Secs,Micro}),
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~w", [Y,Mo,D,H,Mi,S,Micro])).	
