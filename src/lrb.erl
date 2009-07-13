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
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

-export([set_current_file/2, fetch/1, fetch/2, disk_logger_names/0]).

-include("log_roller.hrl").

-define(TIMEOUT, 10000).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link(DiskLoggers) -> {ok,Pid} | ignore | {error,Error}
%%		 DiskLoggers = [disk_logger()]
%% @doc start the server
start_link(DiskLoggers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DiskLoggers, []).
    
%% @spec set_current_file(LoggerName, Index) -> ok
%%		 LoggerName = atom()
%%		 Index = integer()
%% @doc when a log file is closed, refresh it in the cache
set_current_file(LoggerName, Index) when is_atom(LoggerName), is_integer(Index) ->
	gen_server:call(pg2:get_closest_pid(log_roller_browser_grp), {invalidate_cache, LoggerName, Index}, ?TIMEOUT).
	
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
    gen_server:call(pg2:get_closest_pid(log_roller_browser_grp), {fetch, LoggerName, Opts}, ?TIMEOUT).

%% @spec disk_logger_names() -> [atom()]
disk_logger_names() ->
	gen_server:call(pg2:get_closest_pid(log_roller_browser_grp), disk_logger_names, ?TIMEOUT).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init(DiskLoggers) ->
    process_flag(trap_exit, true),
    ok = pg2:create(log_roller_browser_grp),
    ok = pg2:join(log_roller_browser_grp, self()),
	State =
		[begin
			DiskLogger#disk_logger{cache=log_roller_cache:start(DiskLogger#disk_logger.cache_size)}
		 end || DiskLogger <- DiskLoggers],
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({invalidate_cache, LoggerName, Index}, _From, DiskLoggers) ->
	Result = invalidate_cache_internal(disk_logger_by_name(LoggerName, DiskLoggers), Index),
	{reply, Result, DiskLoggers};
	
handle_call({fetch, LoggerName, Opts}, _From, DiskLoggers) ->
	Result = fetch_internal(disk_logger_by_name(LoggerName, DiskLoggers), Opts),
	{reply, Result, DiskLoggers};

handle_call(disk_logger_names, _From, DiskLoggers) ->
	{reply, [Name || #disk_logger{name=Name} <- DiskLoggers], DiskLoggers};
		
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Message, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------	
handle_info(_Info, State) -> io:format("info: ~p~n", [_Info]), {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
	ok.
	
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
disk_logger_by_name(Name, Loggers) -> 
	case lists:keysearch(Name, 2, Loggers) of
		{value, DiskLogger} ->
			DiskLogger;
		false ->
			error_logger:error_report({?MODULE, fetch, disk_logger_not_found, Name}),
			{error, disk_logger_not_found}
	end.
	
invalidate_cache_internal({error, Err}, _) -> {error, Err};

invalidate_cache_internal(Logger, Index) ->
	log_roller_disk_reader:invalidate_cache(Logger#disk_logger.name, Logger#disk_logger.cache, Index).
	
fetch_internal({error, Err}, _) -> {error, Err};

fetch_internal(DiskLogger, Opts) ->
	Max = proplists:get_value(max, Opts, 100),
	UseCache = proplists:get_value(cache, Opts, true),
	Cont = log_roller_disk_reader:start_continuation(DiskLogger#disk_logger.name, DiskLogger#disk_logger.cache, UseCache),
	case fetch_by_continuation(Opts, Max, [], Cont) of
		{ok, Results} -> lists:reverse(Results);
		{error, Reason} -> {error, Reason}
	end.
	
fetch_by_continuation(_, _, _, {error, Err}) -> {error, Err};

fetch_by_continuation(A, B, C, {ok, Cont}) -> fetch_by_continuation(A, B, C, Cont);

fetch_by_continuation(_, Max, Acc, _) when is_integer(Max), length(Acc) >= Max -> {ok, Acc};

fetch_by_continuation(Opts, Max, Acc, Continuation) ->
	case catch log_roller_disk_reader:terms(Continuation) of
		{ok, Continuation1, Terms} ->
			Acc1 = filter(Terms, Opts, Max, Acc),
			fetch_by_continuation(Opts, Max, Acc1, Continuation1);
		{error, read_full_cycle} ->
			{ok, Acc};
		{error, Reason} ->
			{error, Reason};
		{'EXIT', Error} ->
			error_logger:error_report({?MODULE, fetch_internal, Error}),
			{error, Error}
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
