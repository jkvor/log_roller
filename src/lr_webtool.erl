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
-module(lr_webtool).

-export([config_data/0]).
-export([index/2, logs/2]).

-define(WEBTOOL_ARGS, [standard_path, [{port,4057},{bind_address,{0,0,0,0}},{server_name,"localhost"}]]).
-define(TOOL_BASE_URL, "/log_roller/lr_webtool").

config_data() ->
	{log_roller, [{web_data,{"log_roller", ?TOOL_BASE_URL ++ "/index"}}, 
			 {alias,{erl_alias,"/log_roller",[lr_webtool]}}
			]}.

index(_Env,_Input) ->
	display([{"max", "20"}]).

logs(_Env, Input) ->
	display(httpd:parse_query(Input)).
	
display(QS) ->
	io:format("webtool display: ~p~n", [QS]),
	Opts = lists:foldl(
		fun ({_, ""}, Acc0) -> Acc0;
			({"max",Val}, Acc0) -> [{max, list_to_integer(Val)}|Acc0];
			({"type",Val}, Acc0) -> 
				Types = [list_to_atom(Type) || Type <- string:tokens(Val, ",")],
				[{type, Types}|Acc0];
			({"node",Val}, Acc0) -> 
				Nodes = string:tokens(Val, ","),
				[{node, Nodes}|Acc0];
			({Key,Val}, Acc0) -> [{list_to_atom(Key), Val}|Acc0]
		end, [], QS),
		
	io:format("options: ~p~n", [Opts]),
	case (catch log_roller_browser:fetch(Opts)) of
		List when is_list(List) ->
			io:format("fetch success: ~p~n", [List]),
			Header = lr_header:render({data, ?TOOL_BASE_URL, 
						proplists:get_value("max", QS, ""), 
						proplists:get_value("type", QS, "all"), 
						proplists:get_value("node", QS, ""),
						proplists:get_value("grep", QS, "")}),
			Content = lr_logs:render({data, ?TOOL_BASE_URL, List}),
			lr_frame:render({data, ?TOOL_BASE_URL, [Header, Content]});
		Err ->
			io:format("fetch erer: ~p~n", [Err]),
			Err
	end.