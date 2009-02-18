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

-include_lib("stdlib/include/qlc.hrl").
-include("log_roller.hrl").

subscribe_to(Node) when is_list(Node) -> subscribe_to(list_to_atom(Node));
subscribe_to(Node) when is_atom(Node) ->
	gen_server:call(?MODULE, {subscribe_to, Node}).
	
fetch() -> fetch([]).
	
fetch(Opts) when is_list(Opts) ->
	match_objects(Opts).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	ok = db_init(),
    {ok, []}.

db_init() ->
	case (catch mnesia:create_schema([node()])) of
		ok -> 
			ok = mnesia:start(),
		    ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			ok = create_counter_table(),
			ok = create_log_table();
		{error,{_,{already_exists,_}}} -> 
			ok = mnesia:start(),
		    ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			ok = create_counter_table(),
			ok = create_log_table()
	end, ok.

handle_call({subscribe_to, Node}, _From, State) ->
	ok = gen_event:call({error_logger, Node}, log_roller_h, {subscribe, self()}),
	{reply, ok, State};
	
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info({log_roller, _Sender, Log}, State) ->
	io:format("received a log: ~p~n", [Log]),
	case (catch mnesia:dirty_update_counter(log_roller_counter, log_entry, 1)) of
		NextID when is_integer(NextID) ->
			case mnesia:dirty_write(log_roller_logs, Log#log_entry{id=NextID}) of
				ok -> ok;
				Err -> io:format("log write failed: ~p~n", [Err])
			end;
		Err ->
			io:format("log write failed: ~p~n", [Err])
	end,
	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
%% internal

create_counter_table() ->
	case (catch mnesia:create_table(log_roller_counter, 
			[{disc_copies, [node()]}, 
			{attributes, record_info(fields, log_roller_counter)}])) of
		{atomic, ok} -> 
			ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			ok;
		{aborted, Err} -> 
			io:format("counter exists: ~p~n", [Err]), 
			ok
	end.

create_log_table() ->
	case (catch mnesia:create_table(log_roller_logs, 
			[{disc_copies, [node()]}, 
			{type, ordered_set},
		    {record_name, log_entry},
			{attributes, record_info(fields, log_entry)}])) of
		{atomic, ok} -> 
			ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
			ok;
		{aborted, Err} -> 
			io:format("log_roller_logs exists: ~p~n", [Err]), 
			ok
	end.
	
match_objects(Opts) ->
	Max = proplists:get_value(max, Opts, 100),
	
	Type_Fun = 
		fun(Type0) -> 
			case proplists:get_value(type, Opts, all) of
				all -> 
					true;
				Type0 when is_atom(Type0) ->
					true;
				Types when is_list(Types) ->
					lists:member(Type0, Types);
				_ -> 
					false
			end
		end,
		
	Node_Fun =
		fun(Node0) ->
			case proplists:get_value(node, Opts) of
				undefined ->
					true;
				Node0 when is_atom(Node0) ->
					true;
				_ ->
					false
			end
		end,

	QH = qlc:q([X || X <- mnesia:table(log_roller_logs), Type_Fun(X#log_entry.type), Node_Fun(X#log_entry.node)]),
	F = fun() ->
			QC = qlc:cursor(QH),
			case (catch qlc:next_answers(QC, Max)) of
				Res when is_list(Res) ->
					qlc:delete_cursor(QC),
					Res;
				Err ->
					qlc:delete_cursor(QC),
					io:format("error fetching: ~p~n", [Err]),
					[]
			end
		end,
		
	case mnesia:transaction(F) of
		{atomic, Res} ->
			Res;
		Err ->
			Err
	end.
	