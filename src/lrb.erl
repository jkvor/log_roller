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
-compile(export_all).

-export([start_link/1, start/2, server_loop/1]).

-export([
	fetch/2, disk_loggers/0, disk_logger_names/0
]).

-include("log_roller.hrl").

-define(TIMEOUT, infinity).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link([disk_logger()]) -> {ok, pid()}
start_link(DiskLoggers) ->
	proc_lib:start(?MODULE, start, [self(), DiskLoggers]).

%% @spec fetch(Continuation, Opts) -> {continuation(), Results}
%%		 Continuation = atom() | continuation()
%% 		 Opts = [{types, [atom()]} |
%%				 {nodes, [node()]} |
%%				 {grep, string()}]
%%		 Result = list(list(Time::string(), Type::atom(), Node::atom(), Message::any()))
%% @doc fetch a list of log entries for a specific disk_logger
fetch(Continuation, Opts) when is_atom(Continuation); is_record(Continuation, continuation) -> 
	Timeout = proplists:get_value(timeout, Opts, ?TIMEOUT),
	call({fetch, Continuation, Opts}, Timeout).

disk_loggers() ->
	global:send(log_roller_browser_state, {self(), state}),
	receive
		{ok, State} -> State
	after ?TIMEOUT -> exit({state, timeout}) end.

disk_logger_names() ->
	[Logger#disk_logger.name || Logger <- disk_loggers()].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start(Parent, DiskLoggers) ->
	State = [DiskLogger#disk_logger{cache=log_roller_cache:add(DiskLogger#disk_logger.cache_size)} || DiskLogger <- DiskLoggers],
    global:register_name(log_roller_browser_state, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    ?MODULE:server_loop(State).

server_loop(State) ->
	receive
        {From, state} -> From ! {ok, State}
    end,
    server_loop(State).
	
call(Term, Timeout) ->
	Self = self(),
	State = disk_loggers(),
	Pid = spawn_link(fun() -> handle_call(Term, Self, State) end),
	receive
		{Pid, Result} -> Result
	after Timeout -> exit(Pid, timeout) end.

handle_call(Term, From, State) ->
	Self = self(),
	Result = handle_call(Term, State),
	From ! {Self, Result}.
	
handle_call({fetch, LoggerName, Opts}, DiskLoggers) when is_atom(LoggerName) ->
    DiskLogger = disk_logger_by_name(LoggerName, DiskLoggers),
    Continuation = new_continuation(DiskLogger, Opts),
    fetch_by_continuation(Continuation, [], Opts);

handle_call({fetch, Continuation, Opts}, _DiskLoggers) when is_record(Continuation, continuation) ->
	fetch_by_continuation(Continuation, [], Opts).
	
disk_logger_by_name(Name, Loggers) -> 
	case lists:keysearch(Name, 2, Loggers) of
		{value, DiskLogger} ->
			DiskLogger;
		false ->
			error_logger:error_report({?MODULE, fetch, disk_logger_not_found, Name}),
			exit({error, disk_logger_not_found})
	end.

new_continuation(DiskLogger, Opts) ->
	UseCache = proplists:get_value(cache, Opts, true),
	Cache = if UseCache == true -> DiskLogger#disk_logger.cache; true -> undefined end,
	log_roller_disk_reader:start_continuation(DiskLogger#disk_logger.name, Cache, UseCache).
	
fetch_by_continuation(Continuation, Acc, Opts) ->
	case (catch log_roller_disk_reader:terms(Continuation)) of
		{ok, Continuation1, Terms} ->
			NumItems = ?GET_CSTATE(Continuation1, num_items),
			Acc1 = filter(Terms, Opts, Acc),
			{?SET_CSTATE(Continuation1, num_items, NumItems+length(Acc1)), Acc1};
		{'EXIT', {error, read_full_cycle}} ->
			{undefined, Acc};
		{'EXIT', Error} ->
			error_logger:error_report({?MODULE, ?LINE, Error}),
			exit(Error)
	end.
	
filter([], _Opts, Acc) -> lists:reverse(Acc);
filter([LogEntry|Tail], Opts, Acc) ->
	case log_roller_filter:filter(LogEntry, Opts) of
		[] ->
			filter(Tail, Opts, Acc);
		[_] ->
			Term = [format_time(LogEntry#log_entry.time), LogEntry#log_entry.type, LogEntry#log_entry.node, LogEntry#log_entry.message],
			filter(Tail, Opts, [Term|Acc])
	end.
	
format_time({Mega,Secs,Micro}) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time({Mega,Secs,Micro}),
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~w", [Y,Mo,D,H,Mi,S,Micro])).	
