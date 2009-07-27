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
-module(log_roller_web_server).
-behaviour(web_server).

-export([start_link/1, load_server/2]).

%% web_server callbacks
-export([dispatch/2]).

start_link(Args) when is_list(Args) ->
	Args1 = set_app_values([address, port, doc_root], Args),
	web_server:start(?MODULE, Args1).

%%====================================================================
%% web_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: dispatch(Req, PathTokens) -> {reply, Status, Headers, Body} | 
%%										  {reply, Module, Function, Args} | 
%%										  undefined
%%			 Req = mochiweb_request()
%%			 PathTokens = list()
%%--------------------------------------------------------------------	
dispatch(_Req, ['GET', "server"]) ->
	ServerName = lists:nth(1, lrb:disk_logger_names()),
	{reply, ?MODULE, load_server, [[], atom_to_list(ServerName)]};
	
dispatch(Req, [_, "server", ServerName]) ->
	{reply, ?MODULE, load_server, [Req:parse_post(), ServerName]};
	
dispatch(_, _) ->
	undefined.
	
opts([], Dict) -> 
	Dict;
opts([{_, []}|Tail], Dict) ->
	opts(Tail, Dict);
opts([{Key,Val}|Tail], Dict) ->
	Key1 = dict_key(Key),
	Val1 = dict_val(Key1, Val),
	case dict:is_key(Key1, Dict) of
		true ->
			opts(Tail, dict:append_list(Key1, Val1, Dict));
		false ->
			opts(Tail, dict:store(Key1, Val1, Dict))
	end.
	
	
load_server(Opts0, ServerName) ->
    io:format("load_server: ~p~n", [Opts0]),
	Opts = opts(Opts0),
	Result =
		case (catch lrb:fetch(list_to_atom(ServerName), Opts)) of
			List when is_list(List) ->
				%io:format("fetch success: ~p~n", [List]),
				Header = lr_header:render({data, ServerName, lrb:disk_logger_names(),
							proplists:get_value("max", Opts0, ""), 
							proplists:get_value("type", Opts0, "all"), 
							proplists:get_value("node", Opts0, ""),
							proplists:get_value("grep", Opts0, "")}),
				Content = lr_logs:render({data, List}),
				lr_frame:render({data, [Header, Content]});
			{'EXIT', Err} ->
				error_logger:error_report({?MODULE, display, Err}),
	            lists:flatten(io_lib:format("~p~n", [Err]))
		end,
	ok.
%	Response = Req:ok({"text/html", [{"Content-Type", "text/html"}], chunked}),
%	Response:write_chunk("Mochiconntest welcomes you! Your Id: 1\n"),
%	feed(Response, "1", 1).

feed(Response, Path, N) when N == 10 ->
	Msg = io_lib:format("Chunk ~w for id ~s\n", [N, Path]),
    Response:write_chunk(Msg),
	Response;

feed(Response, Path, N) ->
    timer:sleep(1000),
    Msg = io_lib:format("Chunk ~w for id ~s\n", [N, Path]),
    Response:write_chunk(Msg),
    feed(Response, Path, N+1).

%% internal functions
set_app_values([], Args) -> Args;

set_app_values([Key|Tail], Args) ->
	case proplists:get_value(Key, Args) of
		undefined ->
			case application:get_env(log_roller_server, Key) of
				{ok, Val} ->
					set_app_values(Tail, [{Key, Val} | Args]);
				undefined ->
					set_app_values(Tail, Args)
			end;
		_ ->
			set_app_values(Tail, Args)
	end.
	
dict_key("max") -> max;
dict_key("type") -> types;
dict_key("node") -> nodes;
dict_key("grep") -> grep.

dict_val(max, Val) -> 
	case (catch list_to_integer(Val)) of
		{'EXIT', _} -> 0;
		Int -> Int
	end;
dict_val(types, Val) -> [list_to_atom(Val), list_to_atom(Val ++ "_report")];
dict_val(nodes, Val) -> [list_to_atom(Val)];
dict_val(grep, Val) -> Val.