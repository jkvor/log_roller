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

-export([start_link/1, load_server/2, inject_code/1]).

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

dispatch(_Req, ['GET']) ->
    ServerName = lists:nth(1, lrb:disk_logger_names()),
	{reply, ?MODULE, load_server, [[], atom_to_list(ServerName)]};
	
dispatch(_Req, ['GET', "log_roller", "log_roller_webtool", "index"]) ->
    ServerName = lists:nth(1, lrb:disk_logger_names()),
	{reply, ?MODULE, load_server, [[], atom_to_list(ServerName)]};
	
% dispatch(Req, [_, "code_injector"]) ->
%   {reply, ?MODULE, inject_code, [Req:parse_post()]};
	
dispatch(_, _) ->
	undefined.
	
load_server(Opts0, ServerName) ->
    error_logger:info_msg("load_server: ~p~n", [Opts0]),
	Opts = dict:to_list(lists:foldl(
		fun ({_, []}, Dict) ->
				Dict;
			({Key,Val}, Dict) ->
				Key1 = dict_key(Key),
				Val1 = dict_val(Key1, Val),
				case dict:is_key(Key1, Dict) of
					true ->
						dict:append_list(Key1, Val1, Dict);
					false ->
						dict:store(Key1, Val1, Dict)
				end
		end, dict:new(), Opts0)),
	Result =
		case (catch lrb:fetch(list_to_atom(ServerName), Opts)) of
			List when is_list(List) ->
				%error_logger:info_msg("fetch success: ~p~n", [List]),
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
	{reply, 200, [{"Content-Type", "text/html"}], Result}.
	
inject_code([]) ->
	{reply, 200, [{"Content-Type", "text/html"}], lr_code_injector:render({data, "", "", ""})};
	
inject_code(Args) ->
	Node = proplists:get_value("node", Args),
	Code = proplists:get_value("code", Args),
	Response =
		case catch dynamic_compile:from_string("-module(code_injector).\n-compile(export_all).\nexecute() -> \n" ++ Code ++ "\n") of
	        {'EXIT', Error} ->
	            lists:flatten(io_lib:format("error encountered: ~p~n", [Error]));
	        {Module, Bin} ->
	            Result =
	                case rpc:call(list_to_atom(Node), code, load_binary, [Module, "code_injector.erl", Bin]) of
	                    {module, Module} ->
	                        case rpc:call(list_to_atom(Node), Module, execute, []) of
	                            {badrpc, {'EXIT', Error}} ->
	                                Error;
	                            Other ->
	                                Other
	                        end;
	                    {error, Reason} ->
	                        {error, Reason}
	                end,
	            lists:flatten(io_lib:format("~p~n", [Result]));
	        Other ->
	            lists:flatten(io_lib:format("error encountered: ~p~n", [Other]))
	    end,
	{reply, 200, [{"Content-Type", "text/html"}], lr_code_injector:render({data, Node, Code, Response})}.

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