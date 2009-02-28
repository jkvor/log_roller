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
%% @doc Browse log files collected by a log_roller subscriber node.
%% log_roller_browser is a gen_server that is started by the log_roller 
%% application when the "-log_roller_type subscriber" argument is given 
%% in the shell.
-module(log_roller_browser).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%% API exports
-export([fetch/0, fetch/1]).

-include("log_roller.hrl").

%% cache = [{FileNo, Ranges}]
%% Ranges = [{StartByte, StopByte, Logs}]

-record(state, {handles, cache}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec fetch() -> Results
%% @equiv fetch([])
fetch() -> fetch([]).
	
%% @spec fetch(Opts) -> Result
%% 		 Opts = [{max, integer()} |
%%				 {type, atom()} | {type, list(atom())} |
%%				 {node, string()} | {node, list(string())} |
%%				 {grep, string()}]
%%		 Result = list(list(Time::string(), Type::atom(), Node::string(), Message::string()))
%% @doc fetch a list of log entries
fetch(Opts) when is_list(Opts) ->
	gen_server:call(?MODULE, {fetch, Opts}).

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
	{ok, #state{handles=dict:new()}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({fetch, Opts}, _From, State) ->
	{ok, Results, State1} = fetch(State, Opts, proplists:get_value(max, Opts, 100)),
	{reply, lists:reverse(Results), State1};
		
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

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
handle_info(_Info, State) -> {noreply, State}.

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

fetch(State, Opts, Max) -> fetch(State, Opts, Max, [], start).

fetch(State, _Opts, Max, Acc, _Continuation) when length(Acc) >= Max -> {ok, Acc, State};

fetch(#state{handles=Handles}=State, Opts, Max, Acc, Continuation) ->
	io:format("log_roller_browser:fetch(~p, ~p, ~p, ~p, ~p)~n", [State, Opts, Max, Acc, Continuation]),
	{ok, Handles1, Terms, Continuation1} = log_roller_disk_reader:chunk(Handles, Continuation),
	case filter(Terms, Opts, Max, Acc) of
		{ok, Acc1} ->
			fetch(State#state{handles=Handles1}, Opts, Max, Acc1, Continuation1);
		{error, _Reason, Acc1} ->
			{ok, Acc1, State#state{handles=Handles1}}
	end.

filter([], _Opts, _Max, Acc) -> {ok, Acc};

filter(_Results, _Opts, Max, Acc) when length(Acc) >= Max -> {ok, Acc};

filter([{log_entry, Time, Type, Node, Msg}|Tail], Opts, Max, Acc) ->
	Types = proplists:get_all_values(type, Opts),
	Nodes = proplists:get_all_values(node, Opts),
	Grep  = 
		case proplists:get_value(grep, Opts) of
			undefined -> undefined;
			Val ->
				case re:compile(Val) of
					{ok, MP} -> MP;
					_ -> undefined
				end
		end,
	
	Type_Fun = 
		fun(TypeIn) ->
			case Types of
				[] -> true;
				[Types1] when is_list(Types1) ->
					case lists:member(all, Types1) of
						true -> true;
						false ->
							lists:member(TypeIn, Types1)
					end;
				_ ->
					case lists:member(all, Types) of
						true -> true;
						false ->
							lists:member(TypeIn, Types)
					end
			end
		end,
		
	Node_Fun =
		fun(NodeIn) ->
			case Nodes of
				[] -> true;
				_ ->
					lists:member(NodeIn, Nodes)
			end
		end,
		
	Grep_Fun =
		fun(GrepIn) ->
			case Grep of
				undefined -> true;
				_ ->
					Subject = lists:flatten(io_lib:format("~p", [GrepIn])),
					case re:run(Subject,Grep) of
						{match, _} -> true;
						nomatch -> false
					end
			end
		end,
		
	Term = [format_time(Time), Type, Node, Msg],
	case [ Type_Fun(Type), Node_Fun(Node), Grep_Fun(Term) ] of
		[true, true, true] -> 
			filter(Tail, Opts, Max, [Term|Acc]);
		_ -> 
			filter(Tail, Opts, Max, Acc)
	end.
	
format_time({Mega,Secs,Micro}) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time({Mega,Secs,Micro}),
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~w", [Y,Mo,D,H,Mi,S,Micro])).	
