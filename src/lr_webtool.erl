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
-export([index/2, log/2]).

-define(WEBTOOL_ARGS, [standard_path, [{port,4057},{bind_address,{0,0,0,0}},{server_name,"localhost"}]]).
-define(TOOL_BASE_URL, "/log_roller/lr_webtool").

config_data() ->
	{log_roller, [{web_data,{"log_roller", ?TOOL_BASE_URL ++ "/index"}}, 
			 {alias,{erl_alias,"/log_roller",[lr_webtool]}}
			]}.

index(_Env,_Input) ->
	lr_frame:render({data, ?TOOL_BASE_URL, "<H3>Select a node from the list</H3><-- ^_^"}).

log(_Env, Input) ->
	QS0 = httpd:parse_query(Input),
	{Node, QS1} = get_and_remove("name", QS0, ""),
	{View, QS2} = get_and_remove("view", QS1, "list"),
	display(Node, View, QS2).
	
display(Node, View, QS) ->
	Args = lists:foldl(
		fun ({_, ""}, Acc0) -> Acc0;
			({"type",Val}, Acc0) -> [{type, list_to_atom(Val)}|Acc0];
			({"id",Val}, Acc0) -> [{id, list_to_integer(Val)}|Acc0];
			({"max",Val}, Acc0) -> [{max, list_to_integer(Val)}|Acc0];	
			({Key,Val}, Acc0) -> [{list_to_atom(Key), Val}|Acc0]
		end, [], QS),
	case node_call(Node, list_to_atom(View), [Args]) of
		{error,Reason} -> 
			Reason;
		List ->
			Header = lr_header:render({data, ?TOOL_BASE_URL, Node, 
						proplists:get_value("max", QS, ""), 
						proplists:get_value("type", QS, "all"), 
						proplists:get_value("grep", QS, ""), 
						View}
					),
			Content = erlang:apply(list_to_atom("lr_" ++ View), render, [{data, ?TOOL_BASE_URL, Node, lists:reverse(List)}]),
			lr_frame:render({data, ?TOOL_BASE_URL, [Header, Content]})
	end.
	
node_call(Node, Func, Args) ->
	case catch rpc:call(list_to_atom(Node), lrb, Func, Args) of
		{badrpc, Reason} -> {error, Reason};
		Resp when is_binary(Resp) -> binary_to_list(Resp);
		Resp -> Resp
	end.
	
get_and_remove(Key, List, Default) ->
	case lists:keytake(Key, 1, List) of
		{value, {Key, Value}, List1} -> {Value, List1};
		false -> {Default, List}
	end.