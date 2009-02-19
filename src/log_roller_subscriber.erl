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
-module(log_roller_subscriber).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([subscribe_to/1, fetch/0, fetch/1]).

-include("log_roller.hrl").

-record(state, {log}).
-define(LOG_NAME, log_roller_data).

subscribe_to(Node) when is_list(Node) -> subscribe_to(list_to_atom(Node));
subscribe_to(Node) when is_atom(Node) ->
	gen_server:call(?MODULE, {subscribe_to, Node}).
	
%% =============================================================================
%% API FUNCTIONS
%% =============================================================================
fetch() -> fetch([]).
	
fetch(Opts) when is_list(Opts) ->
	gen_server:call(?MODULE, {fetch, Opts}).

%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	Args = [
		{name, ?LOG_NAME},
		%{file, ""}
		{type, wrap},
		{size, {1024, 10}}
	],
	case disk_log:open(Args) of
		{ok, Log} ->
			{ok, #state{log=Log}};
		{repaired, Log, {recovered, _Rec}, {badbytes, _Bad}} ->
			{ok, #state{log=Log}};
		Err ->
			{error, Err}
	end.

handle_call({subscribe_to, Node}, _From, State) ->
	Res = gen_event:call({error_logger, Node}, log_roller_h, {subscribe, self()}),
	{reply, Res, State};

handle_call({fetch, Opts}, _From, #state{log=Log}=State) ->
	Res = chunk(Log, Opts, start, []),
	{reply, Res, State};
		
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info({log_roller, _Sender, Log}, State) ->
	io:format("received a log: ~p~n", [Log]),
	{ok, State1} = write_log(State, Log),
	{noreply, State1};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{log=Log}) -> 
	io:format("closing log~n"),
	disk_log:close(Log).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

write_log(#state{log=Log}=State, LogEntry) ->
	ok = disk_log:log(Log, LogEntry),
	{ok, State}.

chunk(Log, Opts, Continuation, Acc) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			Acc;
		{error, _Reason} ->
			io:format("error: ~p~n", [_Reason]),
			Acc;
		{Continuation1, Terms} ->
			Terms1 = filter(Terms, Opts),
			chunk(Log, Opts, Continuation1, [Terms1|Acc]);
		{Continuation1, Terms, _Badbytes} ->
			Terms1 = filter(Terms, Opts),
			chunk(Log, Opts, Continuation1, [Terms1|Acc])
	end.
	
filter(Terms, Opts) ->
	Type0 = proplists:get_value(type, Opts, all),
	Node0 = proplists:get_value(node, Opts),
	
	Type_Fun = 
		fun(Type) ->
			case Type0 of
				all -> true;
				Type when is_atom(Type) -> true;
				Types when is_list(Types) -> lists:member(Type, Types);
				_ -> false
			end
		end,
		
	Node_Fun =
		fun(Node) ->
			case Node0 of
				undefined -> true;
				Node -> true;
				Nodes when is_list(Nodes) -> lists:member(Node, Nodes);
				_ -> false
			end
		end,
		
	lists:foldl(
		fun({log_entry, _ID, Type, Node, _Time, _Msg} = Term, Acc) ->
			case [ Type_Fun(Type), Node_Fun(Node) ] of
				[true, true] -> [Term|Acc];
				_ -> Acc
			end
		end, [], Terms).
	
format_time({{Y,Mo,D},{H,Mi,S}}) ->
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [Y,Mo,D,H,Mi,S])).	
