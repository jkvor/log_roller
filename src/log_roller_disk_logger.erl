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
-module(log_roller_disk_logger).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

%% API exports
-export([sync/1, register_as_subscriber_with/2, ping/1, total_writes/1, current_location/1, options/1, log/1]).

-include("log_roller.hrl").

-record(state, {log, args, disk_logger_name, filters, total_writes}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc start the server
start_link(DiskLogger) when is_record(DiskLogger, disk_logger) ->
    io:format("start server: ~p~n", [?Server_Name(DiskLogger#disk_logger.name)]),
    gen_server:start_link({local, ?Server_Name(DiskLogger#disk_logger.name)}, ?MODULE, DiskLogger, []).
	
%% @spec sync(LoggerName) -> ok | {error, Reason}
%%       LoggerName = atom()
%% @doc call disk_log:sync() and force flush of cache
sync(LoggerName) when is_atom(LoggerName) ->
	gen_server:call(?Server_Name(LoggerName), sync, infinity).
	
%% @spec register_as_subscriber_with(LoggerName, Node) -> ok
%%		 LoggerName = atom()
%%		 Node = node()
%% @doc send a message to the specified node, registering the gen_server process as a subscriber
register_as_subscriber_with(LoggerName, Node) when is_atom(LoggerName), is_atom(Node) ->
	gen_server:call(?Server_Name(LoggerName), {register_as_subscriber_with, Node}).

%% @spec ping(FromNode) -> [{ok, Pid}]
%%		 FromNode = node()
%%		 Pid = pid()
%% @doc respond to ping requests sent from publisher nodes with the gen_server process id
ping(FromNode) when is_atom(FromNode) ->
    Subscriptions = log_roller_server:determine_subscriptions(),
    lists:foldl(
        fun ({Logger, []}, Acc) ->
                [gen_server:call(?Server_Name(Logger#disk_logger.name), ping)|Acc];
            ({Logger, Nodes}, Acc) ->
	            case lists:member(FromNode, Nodes) of
					true ->
						[gen_server:call(?Server_Name(Logger#disk_logger.name), ping)|Acc];
					false ->
						Acc
				end
	    end, [], Subscriptions).
	
%% @spec total_writes(LoggerName) -> integer()
%%       LoggerName = atom()
total_writes(LoggerName) when is_atom(LoggerName) ->
	gen_server:call(?Server_Name(LoggerName), total_writes).

%% @spec current_location(LoggerName) -> Result
%%       LoggerName = atom()
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
current_location(LoggerName) when is_atom(LoggerName) ->
	gen_server:call(?Server_Name(LoggerName), current_location).
	
%% @spec options(LoggerName) -> Result
%%       LoggerName = atom()
%%       Result = [
%%          {name, DiskLoggerName::atom()},
%%          {file, LogFile::string()},
%%          {type, wrap},
%%          {format, external},
%%          {head, none},
%%          {notify, true},
%%          {size, {MaxBytes::integer(), MaxFiles::integer()}}]
options(LoggerName) when is_atom(LoggerName) ->
	gen_server:call(?Server_Name(LoggerName), options).

log(LoggerName) when is_atom(LoggerName) ->
	gen_server:call(?Server_Name(LoggerName), log).
	
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
init(DiskLogger) when is_record(DiskLogger, disk_logger) ->
	State = initialize_state(DiskLogger),
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
handle_call(sync, _From, #state{log=Log}=State) ->	
	{reply, disk_log:sync(Log), State};
	
handle_call({register_as_subscriber_with, Node}, _From, State) ->
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
		
handle_call(log, _From, #state{log=Log}=State) ->
	{reply, Log, State};
	
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
handle_info({log_roller, _Sender, LogEntry}, #state{log=Log, filters=Filters, total_writes=Writes}=State) ->
	State1 =
		case log_roller_filter:filter(LogEntry, Filters) of
			[] ->
				State;
			[_] ->
				BinLog = term_to_binary(LogEntry),
				LogSize = size(BinLog),
				Bin = <<?Bin_Term_Start/binary, LogSize:16, BinLog:LogSize/binary, ?Bin_Term_Stop/binary>>,
				disk_log:blog(Log, Bin),
				gen_server:abcast(log_roller_tail, {log, Log, LogEntry}),
				State#state{total_writes=Writes+1}			
		end,
	{noreply, State1};

handle_info({_,_,_,{wrap,_NumLostItems}}, #state{log=_Log, disk_logger_name=_Name}=State) ->
	%Infos = disk_log:info(Log),
	%Index = proplists:get_value(current_file, Infos),
	%spawn(fun() -> lrb:set_current_file(Name, Index) end),
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
		
initialize_state(DiskLogger) when is_record(DiskLogger, disk_logger) ->
	LogFile = log_file(DiskLogger),
	Args = [
		{name, DiskLogger#disk_logger.name},
		{file, LogFile},
		{type, wrap},
		{format, external},
		{head, none},
		{notify, true},
		{size, {DiskLogger#disk_logger.maxbytes, DiskLogger#disk_logger.maxfiles}}
	],
	{ok, Log, Args1} = open_log(Args),
	#state{
		log=Log, 
		args=Args1, 
		disk_logger_name=DiskLogger#disk_logger.name,
		filters=DiskLogger#disk_logger.filters, 
		total_writes=0
	}.

log_file(#disk_logger{name=Name, log_dir=Dir}) ->
	case Dir of
		undefined -> atom_to_list(Name);
		_ -> 
			case file:list_dir(Dir) of
				{ok, _} -> 
					Dir ++ "/" ++ atom_to_list(Name);
				{error, enoent} ->
					case file:make_dir(Dir) of
						ok -> 
							Dir ++ "/" ++ atom_to_list(Name);
						DirErr ->
							io:format("failed to create directory ~p: ~p~n", [Dir, DirErr]),
							atom_to_list(Name)
					end
			end
	end.
	
open_log(Args) ->
	Res = disk_log:open(Args),
	io:format("opened log: ~p~n", [Res]),
	case Res of
		{ok, Log} ->
			io:format("info: ~p~n", [disk_log:info(Log)]),
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