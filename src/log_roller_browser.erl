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
-module(log_roller_browser).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([fetch/0, fetch/1]).

-include("log_roller.hrl").

-record(state, {header, handles}).
-define(MAX_CHUNK_SIZE, 65536).
-define(HEADER_SIZE, 14).
		
%% =============================================================================
%% API FUNCTIONS
%% =============================================================================
fetch() -> fetch([]).
	
fetch(Opts) when is_list(Opts) ->
	gen_server:call(?MODULE, {fetch, Opts}).

%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	{ok, #state{header=header_binary(), handles=dict:new()}}.

handle_call({fetch, Opts}, _From, State) ->
	Max = proplists:get_value(max, Opts, 100),
	
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = log_roller_subscriber:current_location(),

	{Index1, Pos1} = rewind_location(Index, Pos, SizeLimit, MaxIndex),
	
	{ok, State1, Res} = chunk(State, {FileStub, Index1, Pos1, SizeLimit, MaxIndex}, Opts, [], Max),
	
	{reply, lists:reverse(Res), State1};
		
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================

chunk(State, _, _, Acc, Max) when length(Acc) >= Max -> {ok, State, Acc};

chunk(#state{header=Header, handles=Handles}=State, {FileStub, Index, Pos, SizeLimit, MaxIndex}, Opts, Acc, Max) ->

	FileName = lists:flatten(io_lib:format("~s.~w", [FileStub, Index])),
	
	{ok, Handles1, IoDevice} = file_handle(FileName, Handles),

	Bytes =
		if 
			Pos + ?MAX_CHUNK_SIZE > SizeLimit ->
				SizeLimit - Pos;
			true -> 
				Pos + ?MAX_CHUNK_SIZE
		end,
		
	%io:format("pos/bytes: ~p and ~p~n", [Pos, Bytes]),
	case file:pread(IoDevice, Pos, Bytes) of
		{ok, Chunk} -> 
			%io:format("chunk: ~p~n", [Chunk]),
			State1 = State#state{handles=Handles1},
			Acc1 = parse_terms(reverse(Chunk), <<>>, Header, Opts, Acc, Max),
			{Index1, Pos1} = rewind_location(Index, Pos, SizeLimit, MaxIndex),
			chunk(State1, {FileStub, Index1, Pos1, SizeLimit, MaxIndex}, Opts, Acc1, Max);
		eof ->
			{ok, State, Acc};
		{error, Reason} -> 
			io:format("error reading file: ~p~n", [Reason]),
			{ok, State, Acc}
	end.
		
rewind_location(Index, Pos, SizeLimit, MaxIndex) ->
	%io:format("rewind location ~p, ~p, ~p, ~p~n", [Index, Pos, SizeLimit, MaxIndex]),
	if
		Pos =:= 0 -> %% move to previous index file
			%io:format("position zero~n"),
			Index1 =
				if
					Index =< 1 -> %% base index, cycle to top index
						MaxIndex;
					true ->
						Index - 1
				end,	
			Pos1 =
				if
					SizeLimit =< ?MAX_CHUNK_SIZE ->
						0;
					true ->
						SizeLimit - ?MAX_CHUNK_SIZE
				end,
			{Index1, Pos1};
		Pos =< ?MAX_CHUNK_SIZE -> %% less than one chunk left
			%io:format("~p =< ~p~n", [Pos, ?MAX_CHUNK_SIZE]),
			{Index, 0};
		true -> %% more than a chunk's worth left
			%io:format("~p - ~p~n", [Pos, ?MAX_CHUNK_SIZE]),
			{Index, Pos - ?MAX_CHUNK_SIZE}
	end.	

			
file_handle(FileName, Handles) ->
	case dict:find(FileName, Handles) of
		{ok, IoDevice} ->
			{ok, Handles, IoDevice};
		error ->
			case file:open(FileName, [read]) of
				{ok, IoDevice} ->
					Handles1 = dict:store(FileName, IoDevice, Handles),
					{ok, Handles1, IoDevice};
				_ ->
					{ok, Handles, error}
			end
	end.

parse_terms(_, _, _, _, Acc, Max) when length(Acc) >= Max -> Acc;

parse_terms(<<>>, _, _, _, Acc, _) -> Acc;
	
parse_terms(<<Header:?HEADER_SIZE/binary, Rest/binary>>, TermAcc, Header, Opts, Acc, Max) ->
	BinTerm = reverse(<<TermAcc/binary, Header/binary>>),
	%io:format("bin term: ~p~n", [BinTerm]),
	case (catch binary_to_term(BinTerm)) of
		Term when is_tuple(Term) ->
			case filter(Term, Opts) of
				false ->
					parse_terms(Rest, <<>>, Header, Opts, Acc, Max);
				Term1 ->
					parse_terms(Rest, <<>>, Header, Opts, [Term1|Acc], Max)
			end;
		Err ->
			io:format("failed converting to term: ~p~n", [Err]),
			Acc
	end;
	
parse_terms(<<A:1/binary, Rest/binary>>, TermAcc, Header, Opts, Acc, Max) ->
	parse_terms(Rest, <<TermAcc/binary, A/binary>>, Header, Opts, Acc, Max).
	
filter({log_entry, Time, Type, Node, Msg}, Opts) ->
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
		[true, true, true] -> Term;
		_ -> false
	end.
	
format_time({Mega,Secs,Micro}) ->
	{{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time({Mega,Secs,Micro}),
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~w", [Y,Mo,D,H,Mi,S,Micro])).	

header_binary() ->
	<<Header:?HEADER_SIZE/binary,_/binary>> = term_to_binary({log_entry,0,0,0,0}),
	reverse(Header).
	
reverse(IoList) when is_list(IoList) ->
	list_to_binary(lists:reverse(lists:flatten(IoList)));
	
reverse(Binary) when is_binary(Binary) -> 
	list_to_binary(lists:reverse(binary_to_list(Binary))).