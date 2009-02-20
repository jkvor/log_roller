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

-export([subscribe_to/1, ping/0, current_location/0]).

-include("log_roller.hrl").

-record(state, {log, args}).
-define(LOG_NAME, log_roller_data).

%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	LogFile =
		case application:get_env(log_roller, log_dir) of
			undefined -> atom_to_list(?LOG_NAME);
			{ok, Dir} -> Dir ++ "/" ++ atom_to_list(?LOG_NAME)
		end,
	Args = [
		{name, ?LOG_NAME},
		{file, LogFile},
		{type, wrap},
		%{size, {10485760, 10}}
		{size, {1048576, 10}}
	],
	case disk_log:open(Args) of
		{ok, Log} ->
			{ok, #state{log=Log, args=Args}};
		{repaired, Log, {recovered, _Rec}, {badbytes, _Bad}} ->
			{ok, #state{log=Log, args=Args}};
		Err ->
			io:format("init error: ~p~n", [Err]),
			{error, Err}
	end.

handle_call({subscribe_to, Node}, _From, State) ->
	Res = gen_event:call({error_logger, Node}, log_roller_h, {subscribe, self()}),
	{reply, Res, State};

handle_call(ping, _From, State) ->
	{reply, {ok, self()}, State};
	
handle_call(current_location, _From, #state{log=Log, args=Args}=State) ->
	Infos = disk_log:info(Log),
	FileStub = proplists:get_value(file, Args),
	Index = proplists:get_value(current_file, Infos),
	Pos = proplists:get_value(no_current_bytes, Infos),
	{SizeLimit, MaxIndex} = proplists:get_value(size, Args),
	{reply, {FileStub, Index, Pos, SizeLimit, MaxIndex}, State};
		
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info({log_roller, _Sender, LogEntry}, #state{log=Log}=State) ->
	io:format("received a log: ~p~n", [term_to_binary(LogEntry)]),
	ok = disk_log:log(Log, LogEntry),
	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{log=Log}) -> 
	io:format("closing log~n"),
	disk_log:close(Log).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =============================================================================
%% API FUNCTIONS
%% =============================================================================
subscribe_to(Node) when is_list(Node) -> subscribe_to(list_to_atom(Node));

subscribe_to(Node) when is_atom(Node) ->
	gen_server:call(?MODULE, {subscribe_to, Node}).

ping() ->
	gen_server:call(?MODULE, ping).
	
current_location() ->
	gen_server:call(?MODULE, current_location).