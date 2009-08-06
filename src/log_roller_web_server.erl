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
-compile(export_all).
%-export([start_link/1]).

-include("log_roller.hrl").

-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_PORT, 8888).
-define(CONTENT_TYPE, {"Content-Type", "text/html"}).

start_link(Args) when is_list(Args) ->
	Address = get_arg_value(address, Args, ?DEFAULT_ADDRESS),
	Port = get_arg_value(port, Args, ?DEFAULT_PORT),
	DocRoot = get_arg_value(doc_root, Args, begin {ok, Dir} = file:get_cwd(), Dir ++ "/public" end),
	Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
	io:format("starting web_server ~p ~p ~p~n", [Address, Port, DocRoot]),
	mochiweb_http:start([{loop, Loop}, {ip, Address}, {port, Port}]).

loop(Req, DocRoot) ->
	PathTokens = [Req:get(method)|string:tokens(Req:get(path), "/")],
	case dispatch(PathTokens) of
		undefined ->
			"/" ++ Path = Req:get(path),
			Req:serve_file(Path, DocRoot);
		Function ->
			erlang:apply(?MODULE, Function, [PathTokens, Req, DocRoot])
	end.
	
dispatch(Path) ->
	case Path of
	    ['GET'] -> main_page;
		[_, "logs"] -> logs;
		['GET', "servers" | _] -> servers;
		['GET', "nodes" | _] -> nodes;
		_ -> undefined
	end.
	
main_page(['GET'], Req, DocRoot) ->
	Req:serve_file("logs.html", DocRoot).
	
logs(['GET', "logs"], Req, _DocRoot) ->
	serve_logs(Req:parse_qs(), Req);
	
logs(['POST', "logs"], Req, _DocRoot) ->
	serve_logs(Req:parse_post(), Req).

servers(['GET', "servers"], Req, _DocRoot) ->
	Content = tab_content(atom_to_list(default_server())),
	Req:respond({200, [?CONTENT_TYPE], Content});

servers(['GET', "servers", ServerName], Req, _DocRoot) ->
	Content = tab_content(ServerName),
	Req:respond({200, [?CONTENT_TYPE], Content}).

nodes(['GET', "nodes"], Req, _DocRoot) ->
	NodeOptions = lr_nodes:render({data, get_nodes(default_server())}),
	Req:respond({200, [?CONTENT_TYPE], NodeOptions});

nodes(['GET', "nodes", Server], Req, _DocRoot) ->
	NodeOptions = lr_nodes:render({data, get_nodes(list_to_atom(Server))}),
	Req:respond({200, [?CONTENT_TYPE], NodeOptions}).
		
serve_logs(Params, Req) ->
	io:format("params: ~p~n", [Params]),
	Dict = opts(Params, dict:new()),
	Max = 
	    case dict:find(max, Dict) of
			{ok, Value1} -> Value1;
			error -> infinity
		end,
	ServerName =
		case dict:find(server, Dict) of
			{ok, Value2} -> Value2;
			error -> default_server()
		end,
	Response = Req:respond({200, [{"Transfer-Encoding", "chunked"}, {"Content-Type", "text/html"}], chunked}),
	fetch_logs(Response, ServerName, dict:to_list(Dict), Max),
	Response:write_chunk("").
		
fetch_logs(Resp, Cont, Opts, Max) ->
	case (catch lrb:fetch(Cont, Opts)) of
		{'EXIT', Error} ->
			error_logger:error_report({?MODULE, ?LINE, Error}),
			ok;
		{Cont1, Logs} ->
			if
			    is_record(Cont1, continuation) ->
			        case ?GET_CSTATE(Cont1, num_items) of
        			    NumItems when is_integer(NumItems), NumItems < Max ->
        			        Content = lr_logs:render({data, Logs}),
        			        Resp:write_chunk(Content),
        			        fetch_logs(Resp, Cont1, Opts, Max);
        			    NumItems when is_integer(NumItems), NumItems >= Max ->
        			        Logs2 = 
            			        case ?GET_CSTATE(Cont, num_items) of
            			            PrevNumItems when is_integer(PrevNumItems) ->
            			                {Logs1, _} = lists:split(Max - PrevNumItems, Logs),
            			                Logs1;
            			            _ ->
            			                Logs
            			        end,
    			            Content = lr_logs:render({data, Logs2}),
        			        Resp:write_chunk(Content)
        			end;
        		true ->
        		    Content = lr_logs:render({data, Logs}),
			        Resp:write_chunk(Content)
        	end
	end.

default_server() ->
	lists:nth(1, lrb:disk_logger_names()).
	
tab_content(ServerName) ->
	lr_tabs:render({data, ServerName, lrb:disk_logger_names()}).

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

%% internal functions
get_arg_value(Key, Args, Default) ->
	case proplists:get_value(Key, Args) of
		undefined -> 
			case application:get_env(log_roller_server, Key) of
				{ok, Val} -> Val;
				undefined -> Default
			end;
		Val -> Val
	end.
	
dict_key("server") -> server;
dict_key("max") -> max;
dict_key("type") -> types;
dict_key("node") -> nodes;
dict_key("grep") -> grep.

dict_val(server, Val) -> list_to_atom(Val);
dict_val(max, Val) -> 
	case (catch list_to_integer(Val)) of
		{'EXIT', _} -> 0;
		Int -> Int
	end;
dict_val(types, Val) -> [list_to_atom(Val), list_to_atom(Val ++ "_report")];
dict_val(nodes, Val) -> [list_to_atom(Val)];
dict_val(grep, Val) -> Val.

get_nodes(Server) ->
	Nodes1 = 
		case lists:filter(fun(#disk_logger{name=Name}) -> Name == Server end, lrb:disk_loggers()) of
			[#disk_logger{filters=Filters}] ->
				case proplists:get_value(nodes, Filters) of
					undefined ->
						[node()|nodes()];
					Nodes ->
						Nodes
				end;
			_ -> 
				[node()|nodes()]
		end,
	lists:sort(Nodes1).
	