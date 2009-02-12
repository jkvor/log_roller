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
-module(log_roller_h).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("log_roller.hrl").

-record(state, {index, wordsize, maxbytes}).

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
			ok = lrb:create_counter_table(),
			ok = lrb:create_config_table(),
			{ok, lrb:lookup_log_index()};
		{error,{_,{already_exists,_}}} -> 
			ok = mnesia:start(),
		    ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			{ok, lrb:lookup_log_index()}
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
	
commit(#state{index=Index0, wordsize=WordSize, maxbytes=MaxBytes}=State, Log) ->
	TableSize = mnesia:table_info(lrb:table_name(Index0), memory),
	{ok, Index} =
		if 
			TableSize * WordSize >= (MaxBytes / ?NUM_TABLES) ->
				Index1 = lrb:next_index(Index0),
				ok = lrb:switch_log_tables(Index1),
				{ok, Index1};	
			true -> 
				{ok, Index0}
		end,
	NextID = mnesia:dirty_update_counter(counter, log_entry, 1),
	io:format("next id: ~p~n", [NextID]),
	ok = mnesia:dirty_write(lrb:table_name(Index), Log#log_entry{id=NextID}),
	{ok, State#state{index=Index}}.

msg(Format, Args) ->
	lists:flatten(io_lib:format(Format, Args)).