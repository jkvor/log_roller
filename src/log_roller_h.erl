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

-record(state, {listening_pids}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------

init(_) ->	
	{ok, #state{listening_pids=[]}}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({error, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=error, node=atom_to_list(node(Pid)), time=erlang:now(), message=msg(Format, Data)}),
	{ok, State1};
	
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=error, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};
	
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};
	
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=warning, node=atom_to_list(node(Pid)), time=erlang:now(), message=msg(Format, Data)}),
	{ok, State1};
		
handle_event({warning_report, _Gleader, {Pid, std_warning, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=warning, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};
		
handle_event({warning_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};
		
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=info, node=atom_to_list(node(Pid)), time=erlang:now(), message=msg(Format, Data)}),
	{ok, State1};
		
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=info, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};
		
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) ->
	{ok, State1} = commit(State, #log_entry{type=Type, node=atom_to_list(node(Pid)), time=erlang:now(), message=Report}),
	{ok, State1};

handle_event(_, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call({subscribe, Pid}, State) ->
	io:format("handle call ~p~n", [Pid]),
	Pids = State#state.listening_pids,
	State1 = State#state{listening_pids=[Pid|Pids]},
	{ok, ok, State1};
	
handle_call(_Request, State) ->
	io:format("handle other call: ~p~n", [_Request]),
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
	
commit(State, Log) ->
	io:format("commit with ~p: ~p~n", [State, Log]),
	ok = broadcast(Log, State#state.listening_pids),
	{ok, State}.

broadcast(_, []) -> ok;
broadcast(Log, [Pid|Tail]) when is_pid(Pid) ->
	Pid ! {log_roller, self(), Log},
	broadcast(Log, Tail).

msg(Format, Args) ->
	lists:flatten(io_lib:format(Format, Args)).