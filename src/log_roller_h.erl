-module(log_roller_h).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(log_roller_config, {key, value}).
-record(log_entry, {type, node, time, message}).
-record(state, {index, wordsize, maxbytes}).

-define(MIN_BYTES, 104856).
-define(NUM_TABLES, 10).
-define(TABLE_NAME, fun(I) when is_integer(I) -> list_to_atom("log_roller_" ++ integer_to_list(I)) end).

%% error_logger:add_report_handler(log_roller, [{maxbytes, 100}])

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([]) -> init([{maxbytes, ?MIN_BYTES}]);

init([{maxbytes, MaxBytes}]) when MaxBytes >= ?MIN_BYTES ->
	{ok, Index} = db_init(),
    {ok, #state{index=Index, wordsize=erlang:system_info(wordsize), maxbytes=MaxBytes}};

init(_) ->
	erlang:error("The value of maxbytes must be greater than 100 kb.").

db_init() ->
	case mnesia:create_schema([node()]) of
		ok -> 
			ok = mnesia:start(),
		    ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			ok = create_config_table(),
			{ok, lookup_log_index()};
		{error,{_,{already_exists,_}}} -> 
			ok = mnesia:start(),
		    ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			{ok, lookup_log_index()}
	end.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=error, node=node(Pid), time=erlang:localtime(), message=msg(Format, Data)}),
	{ok, State1};
	
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=error, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};
	
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};
	
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=warning, node=node(Pid), time=erlang:localtime(), message=msg(Format, Data)}),
	{ok, State1};
		
handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=warning, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};
		
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};
		
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=info, node=node(Pid), time=erlang:localtime(), message=msg(Format, Data)}),
	{ok, State1};
		
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=info, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};
		
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=node(Pid), time=erlang:localtime(), message=Report}),
	{ok, State1};

handle_event(_, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

create_config_table() ->
	case table_exists(log_roller_config) of
		true -> ok;
		false ->
			{atomic, ok} = mnesia:create_table(log_roller_config,
								[{disc_copies, [node()]},
					 		 	 {attributes, record_info(fields, log_roller_config)}])
	end, ok.
			
switch_log_tables(Index) when is_integer(Index) ->	
	TableName = ?TABLE_NAME(Index),
	case table_exists(TableName) of
		true ->
			{atomic, ok} = mnesia:clear_table(TableName);
		false ->
			{atomic, ok} = mnesia:create_table(TableName, 
									[{disc_copies, [node()]},
								      {type, bag},
								      {local_content, true},
								      {record_name, log_entry},
								      {attributes, record_info(fields, log_entry)}])
	end,
	mnesia:dirty_write({log_roller_config, index, Index}).
	
lookup_log_index() ->
	case mnesia:dirty_read(log_roller_config, index) of
		[] -> 
			ok = switch_log_tables(1),
			1;
		[{log_roller_config, index, Index}] -> 
			Index
	end.
		
commit(#state{index=Index0, wordsize=WordSize, maxbytes=MaxBytes}=State, Log) ->
	TableSize = mnesia:table_info(?TABLE_NAME(Index0), memory),
	{ok, Index} =
		if 
			TableSize * WordSize >= (MaxBytes / ?NUM_TABLES) ->
				Index1 = next_index(Index0),
				ok = switch_log_tables(Index1),
				{ok, Index1};	
			true -> 
				{ok, Index0}
		end,
	ok = mnesia:dirty_write(?TABLE_NAME(Index), Log),
	{ok, State#state{index=Index}}.

table_exists(TableName) when is_atom(TableName) ->
	lists:member(TableName, mnesia:system_info(local_tables)).
	
next_index(CurrentIndex) when is_integer(CurrentIndex), CurrentIndex >= ?NUM_TABLES -> 1;
next_index(CurrentIndex) when is_integer(CurrentIndex) -> CurrentIndex + 1.	

msg(Format, Args) ->
	lists:flatten(io_lib:format(Format, Args)).

%write_time(Time) -> write_time(Time, "ERROR REPORT").

%write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
%    io_lib:format("~n=~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===~n", [Type, Y, Mo, D, H, Mi, S]).

%rotate_log(Tablename) ->
%	ok.