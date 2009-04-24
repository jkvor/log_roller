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
%%
%% @doc A webtool module that provides a user interface for log browsing
-module(log_roller_webtool).

-compile(export_all).

-export([compile_templates/0, config_data/0, index/2, server/2, code_injector/2]).

-define(TOOL_BASE_URL, "/log_roller/log_roller_webtool").

compile_templates() ->
	{ok, Filenames} = file:list_dir("templates"),
	[erltl:compile("templates/" ++ Filename, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars]) 
		|| Filename <- Filenames],
	ok.

%% @spec config_data() -> {log_roller, Args::list()}
%% @doc fetch config data for loading module in webtool
config_data() ->
	{log_roller, [{web_data,{"log_roller", ?TOOL_BASE_URL ++ "/index"}}, 
			 {alias,{erl_alias,"/log_roller",[log_roller_webtool]}}
			]}.

index(_Env, _Input) ->
    server(_Env, []).
    
%% @spec index(Env::list(), Input::list()) -> binary()
%% @doc the index function displays the default log view
server(_Env, []) ->
    ServerName = lists:nth(1, lrb:disk_logger_names()),
    display([{"max", "20"}], atom_to_list(ServerName));
    
server(_Env, Input) ->
    io:format("input: ~p~n", [Input]),
	case string:tokens(Input, "?") of
		[ServerName] ->
			display([{"max", "20"}], ServerName);
		[ServerName, QS] ->
			display(httpd:parse_query(QS), ServerName)
	end.
		
display(QS, ServerName) ->
	io:format("webtool display: ~p~n", [QS]),
	Opts0 = lists:foldl(
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
		end, dict:new(), QS),
	Opts = dict:to_list(Opts0),
	io:format("options: ~p~n", [Opts]),
	case (catch lrb:fetch(list_to_atom(ServerName), Opts)) of
		List when is_list(List) ->
			%io:format("fetch success: ~p~n", [List]),
			Header = lr_header:render({data, ?TOOL_BASE_URL, ServerName, lrb:disk_logger_names(),
						proplists:get_value("max", QS, ""), 
						proplists:get_value("type", QS, "all"), 
						proplists:get_value("node", QS, ""),
						proplists:get_value("grep", QS, "")}),
			Content = lr_logs:render({data, ?TOOL_BASE_URL, List}),
			lr_frame:render({data, ?TOOL_BASE_URL, [Header, Content]});
		{'EXIT', Err} ->
			error_logger:error_report({?MODULE, display, Err}),
            lists:flatten(io_lib:format("~p~n", [Err]))
	end.
	
code_injector(_Env, Input) ->
    io:format("injector: ~p, ~p~n", [_Env, Input]),
    Post = httpd:parse_query(Input),
    Node = proplists:get_value("node", Post, ""),
    Code = proplists:get_value("code", Post, ""),
    Result = interpret(list_to_atom(Node), Code),
    io:format("result: ~p~n", [Result]),
    Html = lr_code_injector:render({data, ?TOOL_BASE_URL, Node, Code, Result}),
    io:format("html: ~p~n", [Html]),
    Html.
    
%% log_roller_webtool:interpret('stupid@jacob-vorreuters-macbook-pro-2.local', "hello.").
%% rpc:call('stupid@jacob-vorreuters-macbook-pro-2.local', erlang, apply, [fun(_,_) -> hello end, [test, <<34>>]]).

interpret(_, []) -> "";
    
interpret(Node, Code) ->
    io:format("node: ~p and code: ~p~n", [Node, Code]),
    case catch dynamic_compile:from_string("-module(code_injector).\n-compile(export_all).\nexecute() -> \n" ++ Code ++ "\n") of
        {'EXIT', Error} ->
            lists:flatten(io_lib:format("error encountered: ~p~n", [Error]));
        {Module, Bin} ->
            Result =
                case rpc:call(Node, code, load_binary, [Module, "code_injector.erl", Bin]) of
                    {module, Module1} ->
                        case rpc:call(Node, Module, execute, []) of
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