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

%% API exports
-export([start/1, get/2, put/3, delete/2]).

%% @spec start(CacheSizeInBytes) -> Cache
%%		 CacheSizeInBytes = integer()
%%		 Cache = cherly_cache()
start(CacheSizeInBytes) ->
	{ok, C} = cherly:start(CacheSizeInBytes), C.
	
%% @spec get(string()) -> undefined | any()
get(Cache, Key) when is_tuple(Cache), is_list(Key) ->
	case cherly:get(Cache, Key) of
		not_found -> 
			undefined;
		{ok, Value} -> 
			Value
	end.
		
%% @spec put(list(), binary()) -> ok
put(Cache, Key, Val) when is_tuple(Cache), is_list(Key), is_binary(Val) ->
	case cherly:put(Cache, Key, Val) of
		true ->
			ok;
		Err ->
			io:format("failed to place in cache ~p: ~p~n", [Key, Err]),
			{error, cache_put_failed}
	end;

put(_, _, _) ->
	{error, cache_value_must_be_binary}.

%% @spec delete(string()) -> ok
delete(Cache, Key) when is_tuple(Cache), is_list(Key) ->
	case cherly:remove(Cache, Key) of
		true ->
			ok;
		_ ->
			{error, cache_remove_failed}
	end.