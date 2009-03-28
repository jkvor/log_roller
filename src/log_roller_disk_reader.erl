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

-export([start_continuation/1, invalidate_cache/1, terms/1]).

-include_lib("kernel/include/file.hrl").
-include("log_roller.hrl").

-record(cprops, {file_stub, chunk_size, size_limit, max_index, use_cache}).
-record(cstate, {index, position, last_timestamp, binary_remainder}).
-record(continuation, {properties, state}).
-record(cache_entry, {cstate, terms}).
		
%%====================================================================
%% API
%%====================================================================

%% @spec start_continuation(true | false) -> {ok, continuation()} | {error, string()}
%% @doc fetch a continuation pointed to the log frame currently being written to
start_continuation(UseCache) ->
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = log_roller_disk_logger:current_location(),
	StartPos = snap_to_grid(Pos),
	ChunkSize = Pos-StartPos,
	Props = {cprops, FileStub, ChunkSize, SizeLimit, MaxIndex, UseCache},
	State = {cstate, Index, StartPos, {9999,0,0}, <<>>},
	{ok, {continuation, Props, State}}.

%% @spec invalidate_cache(Index) -> ok | {error, string()}
%% @doc remove all cache entries that point to the file ending in Index
invalidate_cache(Index) ->
	Opts = log_roller_disk_logger:options(),
	{SizeLimit, _} = proplists:get_value(size, Opts),
	Pos = snap_to_grid(SizeLimit),
	invalidate_cache_frame(Index, Pos).

%% @spec terms(Cont) -> {ok, Cont1, Terms} | {error, any()}	
%%		 Cont = continuation()
%% @doc fetch the terms for the continuation passed in
terms(Cont) ->
	case read_chunk(Cont) of
		{ok, Cont1, Terms} ->
			Timestamp1 = (Cont1#continuation.state)#cstate.last_timestamp,
			Timestamp2 = (Cont#continuation.state)#cstate.last_timestamp,
			%% the continuation record stores the last timestamp it encountered
			%% while traveling backward through the log files. If the timestamp
			%% jumps foreward that means all log files have been traversed.
			case is_full_cycle(Timestamp1, Timestamp2) of
				true ->
					{error, read_full_cycle};
				false ->
					{ok, Cont2} = rewind_location(Cont1),
					{ok, Cont2, Terms}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
	
%% lookup cache frames by the key {Index, Pos} and delete
invalidate_cache_frame(Index, Pos) when Pos >= 0 ->
	log_roller_cache:delete({Index, Pos}),
	invalidate_cache_frame(Index, Pos-?MAX_CHUNK_SIZE);
	
invalidate_cache_frame(_, _) -> ok.

%% @spec read_chunk(Cont) -> {ok, Cont1, Terms} | {error, any()}
%% read a chunk either from cache or file
%%	def chunk - a block of binary data containing erlang tuples (log_entry records)
read_chunk(#continuation{properties=Props, state=State}=Cont) ->
    CacheFrame = get_cache_frame(Props#cprops.use_cache, State#cstate.index, State#cstate.position),
    case CacheFrame of
        undefined -> read_chunk_from_file(Cont);
        _ -> read_chunk_from_cache(Cont, CacheFrame)
    end.
    
%% @spec get_cache_frame(UseCache, Index, Pos) -> cache_entry() | undefined
%% fetch the cache frame for the {Index,Pos} key
get_cache_frame(false, _, _) -> undefined;
get_cache_frame(true, Index, Pos) ->
    IsCurrent = is_current_location(Index, Pos),
    if
		IsCurrent -> 
		    %% ignore cache for the frame being written to currently
			undefined;
		true ->
			case log_roller_cache:get({Index, Pos}) of
				undefined -> undefined; %% cache frame does not exist
				CacheEntry -> CacheEntry
			end
	end.
	
%% Fetch the current location from the disk logger.
%% If {Index,Pos} is inside the frame currently being
%% written to then return true, otherwise, false
is_current_location(Index, Pos) ->
    {_, CurrIndex, CurrPos, _, _} = log_roller_disk_logger:current_location(),
    if
		Index == CurrIndex ->
			A = snap_to_grid(Pos),
			B = snap_to_grid(CurrPos),
			A == B;
		true ->
			false
	end.
	
read_chunk_from_file(#continuation{state=State}=Cont) ->
	case read_file(Cont) of
		{ok, Chunk} ->
			BinChunk = list_to_binary(Chunk),
			BinRem = State#cstate.binary_remainder,
			Bin = <<BinChunk/binary, BinRem/binary>>,
			case parse_terms(Bin, <<>>, [], {9999,0,0}) of
				{ok, Terms, BinRem1, LTimestamp1} ->
				    Index = State#cstate.index,
				    Pos = State#cstate.position,
				    State1 = State#cstate{last_timestamp=LTimestamp1, binary_remainder=BinRem1},
				    Cont1 = Cont#continuation{state=State1},
					log_roller_cache:put({Index, Pos}, {cache_entry, State1, Terms}),
					{ok, Cont1, Terms};
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
	
read_chunk_from_cache(#continuation{state=State}=Cont, CacheEntry) ->
    io:format("read from cache {~w, ~w}~n", [State#cstate.index, State#cstate.position]),
    CacheState = CacheEntry#cache_entry.cstate,
    LTimestamp = CacheState#cstate.last_timestamp,
	BinRem = CacheState#cstate.binary_remainder,
	State1 = State#cstate{last_timestamp=LTimestamp, binary_remainder=BinRem},
	{ok, Cont#continuation{state=State1}, CacheEntry#cache_entry.terms}.
	
read_file(#continuation{properties=Props, state=State}) ->
    io:format("read from file {~w, ~w}~n", [State#cstate.index, State#cstate.position]),
	FileName = lists:flatten(io_lib:format("~s.~w", [Props#cprops.file_stub, State#cstate.index])),
	case file_handle(FileName) of
		{ok, IoDevice} ->
			case file:pread(IoDevice, State#cstate.position, Props#cprops.chunk_size) of
				{ok, Chunk} -> 
					{ok, Chunk};
				eof ->
					{ok, []};
				{error, Reason} -> 
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
	
file_handle(FileName) ->
	case log_roller_cache:get(FileName) of
		undefined ->
			case file:open(FileName, [read]) of
				{ok, IoDevice} ->
					log_roller_cache:put(FileName, IoDevice),
					{ok, IoDevice};
				{error, Reason} ->
					{error, Reason}
			end;
		IoDevice ->
			{ok, IoDevice}
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
			case rewind_file_index(FileStub, Index, undefined, MaxIndex) of
				{ok, FileSize, Index1} ->
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
				{error, Reason} ->
					{error, Reason}
			end;
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
rewind_file_index(_FileStub, Index, Index, _MaxIndex) -> {error, cannot_find_next_index};

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
			{error, Reason}
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
					io:format("bad binary data: ~n~p~n", [Bin]),
					{error, bad_binary_data_format}
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