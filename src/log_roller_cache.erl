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
-module(log_roller_cache).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

%% API exports
-export([add/1, get/2, put/3, delete/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec add(integer()) -> CacheStr::string()
add(CacheSizeInBytes) ->
	gen_server:call(?MODULE, {add, CacheSizeInBytes}).

%% @spec get(string()) -> undefined | any()	
get(CacheStr, Key) when is_list(CacheStr), is_list(Key) ->
	gen_server:call(?MODULE, {get, CacheStr, Key}).
	
%% @spec put(list(), binary()) -> ok
put(CacheStr, Key, Val) when is_list(CacheStr), is_list(Key), is_binary(Val) ->
	gen_server:call(?MODULE, {put, CacheStr, Key, Val});
	
put(_, _, _) ->
	exit({error, cache_value_must_be_binary}).

%% @spec delete(string()) -> ok
delete(CacheStr, Key) when is_list(CacheStr), is_list(Key) ->
	gen_server:call(?MODULE, {delete, CacheStr, Key}).
	
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
	{ok, []}.

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
handle_call({add, CacheSizeInBytes}, _From, State) ->
	{ok, Cache} = cherly:start(CacheSizeInBytes),
	CacheStr = cache_to_list(Cache),
	{reply, CacheStr, [{CacheStr, Cache}|State]};
	
handle_call({get, CacheStr, Key}, _From, State) ->
	Cache = proplists:get_value(CacheStr, State),
	{reply, get_internal(Cache, Key), State};

handle_call({put, CacheStr, Key, Val}, _From, State) ->
	Cache = proplists:get_value(CacheStr, State),
	{reply, put_internal(Cache, Key, Val), State};

handle_call({delete, CacheStr, Key}, _From, State) ->
	Cache = proplists:get_value(CacheStr, State),
	{reply, delete_internal(Cache, Key), State}.

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
handle_info(_Info, State) -> error_logger:info_msg("info: ~p~n", [_Info]), {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
cache_to_list({cherly, Port}) ->
	erlang:port_to_list(Port).
	
get_internal(Cache, Key) when is_tuple(Cache), is_list(Key) ->
	case cherly:get(Cache, Key) of
		not_found -> 
			undefined;
		{ok, Value} -> 
			Value
	end.
		
put_internal(Cache, Key, Val) when is_tuple(Cache), is_list(Key), is_binary(Val) ->
	case cherly:put(Cache, Key, Val) of
		true ->
			ok;
		Err ->
			error_logger:info_msg("failed to place in cache ~p: ~p~n", [Key, Err]),
			{error, cache_put_failed}
	end.

delete_internal(Cache, Key) when is_tuple(Cache), is_list(Key) ->
	case cherly:remove(Cache, Key) of
		true ->
			ok;
		_ ->
			{error, cache_remove_failed}
	end.