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
-module(log_roller_disk_reader).
-author('jacob.vorreuter@gmail.com').

-export([chunk/2]).

-include_lib("kernel/include/file.hrl").
-include("log_roller.hrl").

-record(continuation, {file_stub, index, position, size_limit, max_index, last_timestamp, bin_remainder}).

chunk(Handles, start) ->
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = log_roller_disk_logger:current_location(),
	%io:format("start location: ~p, ~p, ~p, ~p, ~p~n", [FileStub, Index, Pos, SizeLimit, MaxIndex]),
	case rewind_location({continuation, FileStub, Index, Pos, SizeLimit, MaxIndex, {9999,0,0}, <<>>}) of
		{ok, Continuation1} ->
			chunk(Handles, Continuation1);
		{error, Reason} ->
			{error, Reason}
	end;
	
chunk(Handles, {continuation, FileStub, Index, Pos, _SizeLimit, _MaxIndex, LTimestamp, BinRem} = Continuation) ->
	%io:format("log_roller_disk_reader:chunk(~p, ~p)~n", [Handles, Continuation]),
	FileName = lists:flatten(io_lib:format("~s.~w", [FileStub, Index])),
	case file_handle(FileName, Handles) of
		{ok, Handles1, IoDevice} ->
			%io:format("pread(~p, ~p, ~p)~n", [IoDevice, Pos, ?MAX_CHUNK_SIZE]),
			case file:pread(IoDevice, Pos, ?MAX_CHUNK_SIZE) of
				{ok, Chunk} -> 
					BinChunk = list_to_binary(Chunk),
					Bin = <<BinChunk/binary, BinRem/binary>>,
					%io:format("chunk: ~p~n", [Bin]),
					case parse_terms(Bin, <<>>, [], {9999,0,0}) of
						{ok, Terms, BinRem1, LTimestamp1} ->
							case full_cycle(LTimestamp1, LTimestamp) of
								true ->
									{error, read_full_cycle};
								false ->
									{ok, Continuation1} = rewind_location(Continuation),
									{ok, Handles1, Continuation1#continuation{last_timestamp=LTimestamp1, bin_remainder=BinRem1}, Terms}
							end;
						{error, Reason} ->
							{error, Reason}
					end;
				eof ->
					{error, eof};
				{error, Reason} -> 
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
		
rewind_location({continuation, FileStub, Index, Pos, SizeLimit, MaxIndex, LTimestamp, BinRem}) ->
	if
		%% file handle was left at beginning of file
		Pos =:= 0 -> 
			%% move to previous index file
			case rewind_file_index(FileStub, Index, undefined, MaxIndex) of
				{ok, FileSize, Index1} ->
					%% set position <chunk size> from end of file	
					Pos1 =
						if
							FileSize > ?MAX_CHUNK_SIZE ->
								FileSize - ?MAX_CHUNK_SIZE;
							true ->
								0
						end,
					{ok, {continuation, FileStub, Index1, Pos1, SizeLimit, MaxIndex, LTimestamp, BinRem}};
				{error, Reason} ->
					{error, Reason}
			end;
		Pos =< ?MAX_CHUNK_SIZE -> %% less than one chunk left
			{ok, {continuation, FileStub, Index, 0, SizeLimit, MaxIndex, LTimestamp, BinRem}};
		true -> %% more than a chunk's worth left
			{ok, {continuation, FileStub, Index, Pos - ?MAX_CHUNK_SIZE, SizeLimit, MaxIndex, LTimestamp, BinRem}}
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
			
file_handle(FileName, Handles) ->
	case dict:find(FileName, Handles) of
		{ok, IoDevice} ->
			{ok, Handles, IoDevice};
		error ->
			case file:open(FileName, [read]) of
				{ok, IoDevice} ->
					Handles1 = dict:store(FileName, IoDevice, Handles),
					{ok, Handles1, IoDevice};
				{error, Reason} ->
					{error, Reason}
			end
	end.

%% @spec parse_terms(Bin, Rem, Acc) -> Result
%%		 Bin = binary()
%%		 Rem = binary()
%%		 Acc = list()
%%		 Result = {ok, Terms, Rem}
parse_terms(<<16#FF:8, 16#FF:8, 16#FF:8, 16#FF:8, LogSize:16/integer, Rest/binary>> = Bin, Rem, Acc, LTimestamp) ->
	%io:format("parse terms: ~p~n", [LogSize]),
	if 
		size(Rest) >= LogSize ->
			case Rest of
				<<Log:LogSize/binary, 16#EE:8, 16#EE:8, 16#EE:8, 16#EE:8, Tail/binary>> ->
					Term = binary_to_term(Log),
					%io:format("term: ~p and tail: ~p~n", [Term, Tail]),
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
	%io:format("parse terms ~p, ~p~n", [A, Rest]),
	parse_terms(Rest, <<Rem/binary, A>>, Acc, LTimestamp).
	
full_cycle({A1,B1,C1}, {A2,B2,C2}) ->
	if 
		A1 >= A2, B1 >= B2, C1 >= C2 -> true;
		true -> false
	end.