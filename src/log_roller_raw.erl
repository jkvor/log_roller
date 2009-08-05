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
%% erl -name log_roller -pa ebin -eval 'application:load(log_roller_server)' -eval 'application:set_env(log_roller_server, default, [{maxbytes, 1024},{maxfiles, 4}])' -eval 'application:start(log_roller_server)' -eval 'application:start(log_roller)'
%% [error_logger:info_msg("ABCD ~w", [I]) || I <- lists:seq(1,10)]
-module(log_roller_raw).
-author('jacob.vorreuter@gmail.com').
-compile(export_all).

read(Log) ->
	Info = disk_log:info(Log),
	case proplists:get_value(type, Info) of
		halt -> undefined;
		wrap ->
			File = proplists:get_value(file, Info),
			{_, NumFiles} = proplists:get_value(size, Info, {0,0}),
			{LeadingBin, Bin} = fast_forward(file(File, 1), <<>>),
			{Terms, Remainder} = feed(Bin, {File, 1}, NumFiles, []),
			case feed_header(1, <<Remainder/binary, LeadingBin/binary>>) of
				{undefined, _} -> Terms;
				{Term1, _} -> [Term1|Terms]
			end
	end.

file(Prefix, Num) ->
	Filename = lists:flatten(lists:concat([Prefix, ".", Num])),
	case file:read_file(Filename) of
		{ok, Binary} -> Binary;
		{error, _Reason} -> <<>>
	end.
	
fast_forward(<<>>, Acc) -> {Acc, <<>>};
fast_forward(<<16#FF:8, 16#FF:8, 16#FF:8, 16#FF:8, _/binary>>=Bin, Acc) -> {Acc, Bin};
fast_forward(<<A:8/binary, Rest/binary>>, Acc) ->
	fast_forward(Rest, <<Acc/binary, A/binary>>).
	
feed(Bin, {Prefix, CurFile}, MaxFile, Acc) ->
	case feed_header(CurFile, Bin) of
		{undefined, Remainder} ->
			if 
				CurFile < MaxFile ->
					Next = file(Prefix, CurFile+1),
					feed(<<Remainder/binary, Next/binary>>, {Prefix, CurFile+1}, MaxFile, Acc);
				true ->
					{Acc, Remainder}
			end;
		{Term, Remainder} ->
			feed(Remainder, {Prefix, CurFile}, MaxFile, [Term|Acc])
	end.
	
feed_header(Num, <<16#FF:8, 16#FF:8, 16#FF:8, 16#FF:8, LogSize:16/integer, Rest/binary>>=Bin) ->
	case Rest of
		<<Body:LogSize/binary, 16#EE:8, 16#EE:8, 16#EE:8, 16#EE:8, Tail/binary>> -> 
			{[Num, binary_to_term(Body)], Tail};
		_ -> 
			{undefined, Bin}
	end;
feed_header(_, Other) -> {undefined, Other}.

