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
%% @doc The subscriber receives log messages broadcast from publisher nodes.
%% log_roller_disk_logger is a gen_server that maintains a file handle
%% for the disk_log that it writes to.
-module(log_roller_disk_logger).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

%% API exports
-export([reload/0, sync/0, subscribe_to/1, ping/0, total_writes/0, current_location/0, options/0]).

-include("log_roller.hrl").

-record(state, {log, args, total_writes}).

-define(LOG_NAME, log_roller_data).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec reload() -> ok
%% @doc reload using config values
reload() ->
	gen_server:call(?MODULE, reload).
	
%% @spec sync() -> ok | {error, Reason}
%% @doc call disk_log:sync() and force flush of cache
sync() ->
	gen_server:call(?MODULE, sync, infinity).
	
%% @spec subscribe_to(Node) -> ok
%%		 Node = list() | node()
%% @doc send a message to the specified node, registering the gen_server process as a subscriber
subscribe_to(Node) when is_list(Node) -> 
	subscribe_to(list_to_atom(Node));

subscribe_to(Node) when is_atom(Node) ->
	gen_server:call(?MODULE, {subscribe_to, Node}).

%% @spec ping() -> {ok, Pid}
%%		 Pid = pid()
%% @doc respond to ping requests sent from publisher nodes with the gen_server process id
ping() ->
	gen_server:call(?MODULE, ping).
	
total_writes() ->
	gen_server:call(?MODULE, total_writes).

%% @spec current_location() -> Result
%%		 Result = {FileStub, Index, Pos, SizeLimit, MaxIndex}
%%		 FileStub = list()
%%		 Index = integer()
%%		 Pos = integer()
%%		 SizeLimit = integer()
%%		 MaxIndex = integer()
%% @doc return the current location details of the disk_log
%% FileStub is the base file name which Index is appended to
%% when creating the logs. ie: log_roller_data.1
%% Pos is the position in the current log file after the last
%% insert.  SizeLimit and MaxIndex are the config values that
%% dictate how large log files can become and how many files
%% to distribute the logs amongst.
current_location() ->
	gen_server:call(?MODULE, current_location).
	
options() ->
	gen_server:call(?MODULE, options).
	
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
init(_) ->
	State = initialize_state(),
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
handle_call(reload, _From, #state{log=Log}) ->
	disk_log:close(Log),
	{reply, ok, initialize_state()};

handle_call(sync, _From, #state{log=Log}=State) ->	
	{reply, disk_log:sync(Log), State};
	
handle_call({subscribe_to, Node}, _From, State) ->
	Res = gen_event:call({error_logger, Node}, log_roller_h, {subscribe, self()}),
	{reply, Res, State};

handle_call(ping, _From, State) ->
	{reply, {ok, self()}, State};

handle_call(total_writes, _From, State) ->
	{reply, State#state.total_writes, State};	
	
handle_call(current_location, _From, #state{log=Log, args=Args}=State) ->
	Infos = disk_log:info(Log),
	FileStub = proplists:get_value(file, Args),
	Index = proplists:get_value(current_file, Infos),
	Pos = proplists:get_value(no_current_bytes, Infos),
	{SizeLimit, MaxIndex} = proplists:get_value(size, Args),
	{reply, {FileStub, Index, Pos, SizeLimit, MaxIndex}, State};
	
handle_call(options, _From, #state{args=Args}=State) ->
	{reply, Args, State};
		
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
handle_info({log_roller, _Sender, LogEntry}, #state{log=Log, total_writes=Writes}=State) ->
	BinLog = term_to_binary(LogEntry),
	LogSize = size(BinLog),
	Bin = <<?Bin_Term_Start/binary, LogSize:16, BinLog:LogSize/binary, ?Bin_Term_Stop/binary>>,
	%io:format("log for ~p: ~p~n", [Log, Bin]),
	ok = disk_log:blog(Log, Bin),
	{noreply, State#state{total_writes=Writes+1}};

handle_info({_,_,_,{wrap,_NumLostItems}}, #state{log=Log}=State) ->
	Infos = disk_log:info(Log),
	Index = proplists:get_value(current_file, Infos),
	spawn(fun() -> log_roller_browser:set_current_file(Index) end),
	{noreply, State};
	
handle_info(_Info, State) -> io:format("info: ~p~n", [_Info]), {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, #state{log=Log}) -> 
	io:format("closing log~n"),
	disk_log:close(Log).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

log_file() ->
	case application:get_env(log_roller_subscriber, log_dir) of
		undefined -> atom_to_list(?LOG_NAME);
		{ok, Dir} -> 
			case file:list_dir(Dir) of
				{ok, _} -> 
					Dir ++ "/" ++ atom_to_list(?LOG_NAME);
				{error, enoent} ->
					case file:make_dir(Dir) of
						ok -> 
							Dir ++ "/" ++ atom_to_list(?LOG_NAME);
						DirErr ->
							io:format("failed to create directory ~p: ~p~n", [Dir, DirErr]),
							atom_to_list(?LOG_NAME)
					end
			end
	end.
	
initialize_state() ->
	LogFile = log_file(),
	Maxbytes = 
		case application:get_env(log_roller_subscriber, maxbytes) of
			undefined -> 10485760;
			{ok, Val1} -> Val1
		end,
	Maxfiles = 
		case application:get_env(log_roller_subscriber, maxfiles) of
			undefined -> 10;
			{ok, Val2} -> Val2
		end,
	Args = [
		{name, ?LOG_NAME},
		{file, LogFile},
		{type, wrap},
		{format, external},
		{head, none},
		{notify, true},
		{size, {Maxbytes, Maxfiles}}
	],
	{ok, Log, Args1} = open_log(Args),
	#state{log=Log, args=Args1, total_writes=0}.
	
open_log(Args) ->
	case disk_log:open(Args) of
		{ok, Log} ->
			%io:format("open log ~p for ~p~n", [Log, Args]),
			{ok, Log, Args};
		{repaired, Log, {recovered, _Rec}, {badbytes, _Bad}} ->
			{ok, Log, Args};
		{error,{file_error,_,eacces}} ->
			io:format("insufficient permission level to open ~s~n", [proplists:get_value(file)]),
			exit(eacces);
		{error,{size_mismatch,_,NewSize}} ->
			Args1 = proplists:delete(size, Args),
			{ok, Log1, Args2} = open_log(Args1),
			case disk_log:change_size(Log1, NewSize) of
				ok ->
					{ok, Log1, [{size, NewSize}|Args2]};
				Err ->
					io:format("init error: ~p~n", [Err]),
					exit(Err)
			end;
		Err ->
			io:format("init error: ~p~n", [Err]),
			exit(Err)
	end.