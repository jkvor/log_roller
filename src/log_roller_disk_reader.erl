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

-record(continuation, {file_stub, index, position, size_limit, max_index, bin_remainder}).

chunk(Handles, start) ->
	{FileStub, Index, Pos, SizeLimit, MaxIndex} = log_roller_disk_logger:current_location(),
	io:format("start location: ~p, ~p, ~p, ~p, ~p~n", [FileStub, Index, Pos, SizeLimit, MaxIndex]),
	case rewind_location({continuation, FileStub, Index, Pos, SizeLimit, MaxIndex, <<>>}) of
		{ok, Continuation1} ->
			chunk(Handles, Continuation1);
		{error, Reason} ->
			{error, Reason}
	end;
	
chunk(Handles, {continuation, FileStub, Index, Pos, _SizeLimit, _MaxIndex, BinRem} = Continuation) ->
	io:format("log_roller_disk_reader:chunk(~p, ~p)~n", [Handles, Continuation]),
	FileName = lists:flatten(io_lib:format("~s.~w", [FileStub, Index])),
	case file_handle(FileName, Handles) of
		{ok, Handles1, IoDevice} ->
			io:format("pread(~p, ~p, ~p)~n", [IoDevice, Pos, ?MAX_CHUNK_SIZE]),
			case file:pread(IoDevice, Pos, ?MAX_CHUNK_SIZE) of
				{ok, Chunk} -> 
					io:format("chunk: ~p~n", [Chunk]),
					{ok, Terms, BinRem1} = parse_terms(reverse(<<Chunk/binary, BinRem/binary>>), []),
					{ok, Continuation1} = rewind_location(Continuation),
					{ok, Handles1, Continuation1#continuation{bin_remainder=BinRem1}, Terms};
				eof ->
					{error, eof};
				{error, Reason} -> 
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
		
rewind_location({continuation, FileStub, Index, Pos, SizeLimit, MaxIndex, BinRem}) ->
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
					{ok, {continuation, FileStub, Index1, Pos1, SizeLimit, MaxIndex, BinRem}};
				{error, Reason} ->
					{error, Reason}
			end;
		Pos =< ?MAX_CHUNK_SIZE -> %% less than one chunk left
			{ok, {continuation, FileStub, Index, 0, SizeLimit, MaxIndex, BinRem}};
		true -> %% more than a chunk's worth left
			{ok, {continuation, FileStub, Index, Pos - ?MAX_CHUNK_SIZE, SizeLimit, MaxIndex, BinRem}}
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

%% @spec parse_terms(Bin, Acc) -> Result
%%		 Bin = binary()
%%		 Acc = list()
%%		 Result = {ok, Terms, Rem}
parse_terms(<<16#EE:1, 16#EE:1, 16#EE:1, 16#EE:1, LogSize:2, Rest/binary>> = Bin, Acc) ->
	if 
		size(Rest) >= LogSize ->
			<<Log:LogSize, 16#FF, 16#FF, 16#FF, 16#FF, Tail/binary>> = Rest,
			Term = binary_to_term(Log),
			parse_terms(Tail, [Term|Acc]);
		true ->
			{ok, Acc, Bin}
	end.

reverse(IoList) when is_list(IoList) ->
	list_to_binary(lists:reverse(lists:flatten(IoList)));
	
reverse(Binary) when is_binary(Binary) -> 
	list_to_binary(lists:reverse(binary_to_list(Binary))).