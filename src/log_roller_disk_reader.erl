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
-module(log_roller_disk_reader).
-author('jacob.vorreuter@gmail.com').

-export([start_continuation/3, invalidate_cache/3, terms/1]).

-include_lib("kernel/include/file.hrl").
-include("log_roller.hrl").
		
%%====================================================================
%% API
%%====================================================================

%% @spec start_continuation(atom(), {cherly, post()}, true | false) -> {ok, continuation()}
%% @doc fetch a continuation pointed to the log frame currently being written to
start_continuation(LoggerName, Cache, UseCache) ->
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = log_roller_disk_logger:current_location(LoggerName),
	StartPos = snap_to_grid(Pos),
	ChunkSize = Pos-StartPos,
	Props = {cprops, LoggerName, FileStub, ChunkSize, SizeLimit, MaxIndex, UseCache},
	State = {cstate, Index, StartPos, {9999,0,0}, <<>>, Cache, 0},
	{ok, {continuation, Props, State}}.

%% @spec invalidate_cache(atom(), {cherly, port()}, Index) -> ok
%% @doc remove all cache entries that point to the file ending in Index
invalidate_cache(LoggerName, Cache, Index) ->
	Opts = log_roller_disk_logger:options(LoggerName),
	{SizeLimit, _} = proplists:get_value(size, Opts),
	Pos = snap_to_grid(SizeLimit),
	invalidate_cache_frame(Cache, Index, Pos).

%% @spec terms(Cont) -> {ok, Cont1, Terms}
%%		 Cont = continuation()
%% @doc fetch the terms for the continuation passed in
terms(Cont) ->
	{ok, Cont1, Terms} = read_chunk(Cont),
	Timestamp1 = (Cont1#continuation.state)#cstate.last_timestamp,
	Timestamp2 = (Cont#continuation.state)#cstate.last_timestamp,
	%% the continuation record stores the last timestamp it encountered
	%% while traveling backward through the log files. If the timestamp
	%% jumps foreward that means all log files have been traversed.
	case is_full_cycle(Timestamp1, Timestamp2) of
		true ->
			exit({error, read_full_cycle});
		false ->
			{ok, Cont2} = rewind_location(Cont1),
			{ok, Cont2, Terms}
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
%% lookup cache frames by the key {Index, Pos} and delete
invalidate_cache_frame(Cache, Index, Pos) when Pos >= 0 ->
	log_roller_cache:delete(Cache, key({Index, Pos})),
	invalidate_cache_frame(Cache, Index, Pos-?MAX_CHUNK_SIZE);
	
invalidate_cache_frame(_, _, _) -> ok.

%% @spec read_chunk(Cont) -> {ok, Cont1, Terms}
%% @doc read a chunk either from cache or file
%%	def chunk - a block of binary data containing erlang tuples (log_entry records)
read_chunk(#continuation{properties=Props, state=State}=Cont) ->
    CacheFrame = get_cache_frame(Props#cprops.disk_logger_name, State#cstate.cache, Props#cprops.use_cache, State#cstate.index, State#cstate.position),
    case CacheFrame of
        undefined -> read_chunk_from_file(Cont);
        _ -> read_chunk_from_cache(Cont, CacheFrame)
    end.
    
%% @spec get_cache_frame(LoggerName, UseCache, Index, Pos) -> cache_entry() | undefined
%% @doc fetch the cache frame for the {Index,Pos} key
get_cache_frame(_, _, false, _, _) -> undefined;
get_cache_frame(LoggerName, Cache, true, Index, Pos) ->
    IsCurrent = is_current_location(LoggerName, Index, Pos),
    if
		IsCurrent -> 
		    %% ignore cache for the frame being written to currently
			undefined;
		true ->
			case log_roller_cache:get(Cache, key({Index, Pos})) of
				undefined -> undefined; %% cache frame does not exist
				CacheEntry -> 
					io:format("got from cache: ~p~n", [{Index, Pos}]),
					binary_to_term(CacheEntry)
			end
	end.
	
%% Fetch the current location from the disk logger.
%% If {Index,Pos} is inside the frame currently being
%% written to then return true, otherwise, false
is_current_location(LoggerName, Index, Pos) ->
    {_, CurrIndex, CurrPos, _, _} = log_roller_disk_logger:current_location(LoggerName),
    if
		Index == CurrIndex ->
			A = snap_to_grid(Pos),
			B = snap_to_grid(CurrPos),
			A == B;
		true ->
			false
	end.
	
read_chunk_from_file(#continuation{state=State}=Cont) ->
	{ok, Chunk} = read_file(Cont),
	BinChunk = list_to_binary(Chunk),
	BinRem = State#cstate.binary_remainder,
	Bin = <<BinChunk/binary, BinRem/binary>>,
	{ok, Terms, BinRem1, LTimestamp1} = parse_terms(Bin, <<>>, [], {9999,0,0}),
    Index = State#cstate.index,
    Pos = State#cstate.position,
    State1 = State#cstate{last_timestamp=LTimestamp1, binary_remainder=BinRem1},
    Cont1 = Cont#continuation{state=State1},
	log_roller_cache:put(State#cstate.cache, key({Index, Pos}), term_to_binary({cache_entry, State1, Terms})),
	{ok, Cont1, Terms}.
	
read_chunk_from_cache(#continuation{state=State}=Cont, CacheEntry) ->
    %io:format("read from cache {~w, ~w}~n", [State#cstate.index, State#cstate.position]),
    CacheState = CacheEntry#cache_entry.cstate,
    LTimestamp = CacheState#cstate.last_timestamp,
	BinRem = CacheState#cstate.binary_remainder,
	State1 = State#cstate{last_timestamp=LTimestamp, binary_remainder=BinRem},
	{ok, Cont#continuation{state=State1}, CacheEntry#cache_entry.terms}.
	
read_file(#continuation{properties=Props, state=State}) ->
    %io:format("read from file {~w, ~w}~n", [State#cstate.index, State#cstate.position]),
	FileName = lists:flatten(io_lib:format("~s.~w", [Props#cprops.file_stub, State#cstate.index])),
	{ok, IoDevice} = file_handle(State#cstate.cache, FileName),
	case file:pread(IoDevice, State#cstate.position, Props#cprops.chunk_size) of
		{ok, Chunk} -> 
			{ok, Chunk};
		eof ->
			{ok, []};
		{error, Reason} -> 
			exit({error, Reason})
	end.
	
file_handle(Cache, FileName) ->
	case log_roller_cache:get(Cache, FileName) of
		undefined ->
			case file:open(FileName, [read]) of
				{ok, IoDevice} ->
					log_roller_cache:put(Cache, FileName, term_to_binary(IoDevice)),
					{ok, IoDevice};
				{error, Reason} ->
					exit({error, Reason})
			end;
		IoDevice ->
			{ok, binary_to_term(IoDevice)}
	end.
		
rewind_location(#continuation{properties=Props, state=State}=Cont) ->
    FileStub = Props#cprops.file_stub,
    MaxIndex = Props#cprops.max_index,
    Index = State#cstate.index,
    Pos = State#cstate.position,
	if
		%% file handle was left at beginning of file
		Pos =:= 0 -> 
			%% move to previous index file
			{ok, FileSize, Index1} = rewind_file_index(FileStub, Index, undefined, MaxIndex),
			{Pos1,ChunkSize1} =
				if
					FileSize > ?MAX_CHUNK_SIZE ->
						P1 = snap_to_grid(FileSize),
						{P1, (FileSize - P1)};
					true ->
						{0, ?MAX_CHUNK_SIZE}
				end,
			Props1 = Props#cprops{chunk_size=ChunkSize1},
			State1 = State#cstate{index=Index1, position=Pos1},
			{ok, Cont#continuation{properties=Props1, state=State1}};
		Pos =< ?MAX_CHUNK_SIZE -> %% less than one chunk left
		    Props1 = Props#cprops{chunk_size=?MAX_CHUNK_SIZE},
			State1 = State#cstate{position=0},
			{ok, Cont#continuation{properties=Props1, state=State1}};
		true -> %% more than a chunk's worth left
			Pos1 = snap_to_grid(Pos - ?MAX_CHUNK_SIZE),
			Props1 = Props#cprops{chunk_size=(Pos-Pos1)},
			State1 = State#cstate{position=Pos1},
			{ok, Cont#continuation{properties=Props1, state=State1}}
	end.	

%% if Index and StartingIndex match then we've cycled all the way around
rewind_file_index(_FileStub, Index, Index, _MaxIndex) -> exit({error, cannot_find_next_index});

rewind_file_index(FileStub, Index, StartingIndex, MaxIndex) ->
	Index1 =
		if
			Index =< 1 -> %% base index, cycle to top index
				MaxIndex;
			true ->
				Index - 1
		end,
	FileName = lists:flatten(io_lib:format("~s.~w", [FileStub, Index1])),
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			{ok, FileInfo#file_info.size, Index1};
		{error, enoent} ->
			case StartingIndex of
				undefined ->
					rewind_file_index(FileStub, Index1, Index, MaxIndex);
				_ ->
					rewind_file_index(FileStub, Index1, StartingIndex, MaxIndex)
			end;
		{error, Reason} ->
			exit({error, Reason})
	end.
			
%% @spec parse_terms(Bin, Rem, Acc, LTimestamp) -> Result
%%		 Bin = binary()
%%		 Rem = binary()
%%		 Acc = list()
%%		 LTimestamp = tuple()
%%		 Result = {ok, Terms, Rem}
parse_terms(<<16#FF:8, 16#FF:8, 16#FF:8, 16#FF:8, LogSize:16/integer, Rest/binary>> = Bin, Rem, Acc, LTimestamp) ->
	if 
		size(Rest) >= LogSize ->
			case Rest of
				<<Log:LogSize/binary, 16#EE:8, 16#EE:8, 16#EE:8, 16#EE:8, Tail/binary>> ->
					Term = binary_to_term(Log),
					parse_terms(Tail, Rem, [Term|Acc], Term#log_entry.time);
				_ ->
					error_logger:info_msg("bad binary data: ~n~p~n", [Bin]),
					exit({error, bad_binary_data_format})
			end;
		true ->
			{ok, Acc, Bin, LTimestamp}
	end;
	
parse_terms(<<>>, Rem, Acc, LTimestamp) ->
	{ok, Acc, Rem, LTimestamp};
	
parse_terms(<<A:8, Rest/binary>>, Rem, Acc, LTimestamp) ->
	parse_terms(Rest, <<Rem/binary, A>>, Acc, LTimestamp).
		
snap_to_grid(Position) ->
	((Position div ?MAX_CHUNK_SIZE) * ?MAX_CHUNK_SIZE).
	
is_full_cycle({A1,B1,C1}, {A2,B2,C2}) ->
	if 
		A1 =:= A2, B1 =:= B2, C1 >= C2 -> true;
		A1 =:= A2, B1 > B2 -> true;
		A1 > A2 -> true;
		true -> false
	end.
	
key({A, B}) when is_integer(A), is_integer(B) -> 
	lists:flatten(io_lib:format("~w_~w", [A,B]));
key(_) ->
	exit({error, unexpected_key}).