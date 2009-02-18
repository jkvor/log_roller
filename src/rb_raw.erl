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
%% ORIGINAL LICENSE FROM rb.erl:
%%
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
-module(rb_raw).

%% External exports
-export([list/0, list/1]).
-export([show/0, show/1]).

show() -> show([]).

show(Opts) when is_list(Opts) ->
	Logs = list(Opts),
	[show(Log) || Log <- Logs];
	
show({_No, _Type, _Descr, _Date, Fname, FilePosition}) ->
	Dir = get_report_dir(),
	FileName = lists:concat([Dir, "/", Fname]),
    case file:open(FileName, [read]) of
		{ok, Fd} -> 
		    read_rep_msg(Fd, FilePosition);
		_ -> 
		    io:format("rb: can't open file ~p~n", [FileName])
    end.

list() -> list([]).

list(Opts) when is_list(Opts) ->
	Max = proplists:get_value(max, Opts, 100),
	Type = proplists:get_value(type, Opts, all),
	Dir = 
		case proplists:get_value(report_dir, Opts) of
			undefined -> get_report_dir();
			Dir0 -> Dir0
		end,
	list(Max, Type, Dir).
		
%% internal

list(Max, Type, Dir) when is_integer(Max), is_atom(Type), is_list(Dir) ->
	scan_files(Dir ++ "/", Max, Type).

get_report_dir() ->
	case catch application:get_env(sasl, error_logger_mf_dir) of
		{ok, Dir} -> 
			Dir;
		_ ->
	    	exit("cannot locate report directory")
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max, Type)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%%               to read.
%%       Type ::= atom(), describing which reports to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements 
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max, Type) ->
    case file:open(RptDir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [LastWritten]} -> 
		    Files = make_file_list(RptDir, LastWritten),
		    scan_files(RptDir, Files, Max, Type);		
		_ -> exit("cannot read the index file")
	    end;
	_ -> exit("cannot read the index file")
    end.

make_file_list(Dir, FirstFileNo) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    FileNumbers = lists:zf(fun(Name) ->
					   case catch list_to_integer(Name) of
					       Int when is_integer(Int) ->
						   {true, Int};
					       _ ->
						   false
					   end
				   end,
				   FileNames),
	    shift(lists:sort(FileNumbers), FirstFileNo);
	_ -> exit({bad_directory, Dir})
    end.
					  
shift(List, First) -> 
    shift(List, First, []).

shift([H | T], H, Res) ->
    [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
    shift(T, First, [H | Res]);
shift([], _, Res) ->
    Res.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max, Type)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max, Type) ->
    scan_files(Dir, 1, Files, [], Max, Type).
scan_files(_Dir, _, [], Res, _Max, _Type) -> Res;
scan_files(_Dir, _, _Files, Res, Max, _Type) when Max =< 0 -> Res;
scan_files(Dir, No, [H|T], Res, Max, Type) ->
    Data = get_report_data_from_file(Dir, No, H, Max, Type),
    Len = length(Data),
    NewMax = dec_max(Max, Len),
    NewNo = No + Len,
    NewData = Data ++ Res,
    scan_files(Dir, NewNo, T, NewData, NewMax, Type).

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max, Type) ->	
    Fname = integer_to_list(FileNr),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> read_reports(No, Fd, Fname, Max, Type);
	_ -> [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max, Type)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max, Type) ->
    io:format("rb: reading report..."),
    case catch read_reports(Fd, [], Type) of
	{ok, Res} -> 
	    file:close(Fd),
	    io:format("done.~n"),
	    NewRes = 
		if
		    length(Res) > Max ->
			lists:sublist(Res, 1, Max);
		    true ->
			Res
		end,
	    add_report_data(NewRes, No, Fname);
	{error, [Problem | Res]} ->
	    file:close(Fd),
	    io:format("Error: ~p~n",[Problem]),
	    io:format("Salvaged ~p entries from corrupt report file ~s...~n",
		      [length(Res),Fname]),
	    NewRes = 
		if
		    length([Problem|Res]) > Max ->
			lists:sublist([Problem|Res], 1, Max);
		    true ->
			[Problem|Res]
		end,
	    add_report_data(NewRes, No, Fname);
	Else ->
	    io:format("err ~p~n", [Else]),
	    [{No, unknown, "Can't read reports from file " ++ Fname,
		  "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
    add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
    add_report_data(T, No+1, FName,
		    [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], _No, _FName, Res) -> Res.

read_reports(Fd, Res, Type) ->
    {ok, FilePos} = file:position(Fd, cur),
    case catch read_report(Fd) of
	{ok, Report} -> 
	    RealType = get_type(Report),
	    {ShortDescr, Date} = get_short_descr(Report),
	    Rep = {RealType, ShortDescr, Date, FilePos},
	    if
		Type == all->
		    read_reports(Fd, [Rep | Res], Type);
		RealType == Type ->
		    read_reports(Fd, [Rep | Res], Type);
		is_list(Type) ->
		    case lists:member(RealType, Type) of
			true ->
			    read_reports(Fd, [Rep | Res], Type);
			_ ->
			    read_reports(Fd, Res, Type)
		    end;
		true ->
		    read_reports(Fd, Res, Type)
	    end;
	{error, Error} ->
	    {error, [{unknown, Error, [], FilePos} | Res]};
	eof ->
	    {ok, Res};
	{'EXIT', Reason} ->
	    [{unknown, Reason, [], FilePos} | Res]
    end.

read_report(Fd) ->
    case io:get_chars(Fd,'',2) of
        [Hi,Lo] ->
            Size = get_int16(Hi,Lo),
            case io:get_chars(Fd,'',Size) of
                eof ->
                    {error,"Premature end of file"};
                List ->
                    Bin = list_to_binary(List),
		    Ref = make_ref(),
		    case (catch {Ref,binary_to_term(Bin)}) of
			{'EXIT',_} ->
			    {error, "Inclomplete erlang term in log"};
			{Ref,Term} ->
			    {ok, Term}
		    end
	    end;
        eof ->
            eof
    end.
 
get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({_Time, {error_report, _Pid, {_, crash_report, _}}}) ->
    crash_report;
get_type({_Time, {error_report, _Pid, {_, supervisor_report, _}}}) ->
    supervisor_report;
get_type({_Time, {info_report, _Pid, {_, progress, _}}}) ->
    progress;
get_type({_Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({{Date, Time}, {error_report, Pid, {_, crash_report, Rep}}}) ->
    [OwnRep | _] = Rep,
    Name = 
	case lists:keysearch(registered_name, 1, OwnRep) of
	    {value, {_Key, []}} ->
		case lists:keysearch(initial_call, 1, OwnRep) of
		    {value, {_K, {M,_F,_A}}} -> M;
		    _ -> Pid
		end;
	    {value, {_Key, N}} -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date, Time)};
get_short_descr({{Date, Time}, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
    Name =
	case lists:keysearch(supervisor, 1, Rep) of
	    {value, {_Key, N}} when is_atom(N) -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date,Time)};
get_short_descr({{Date, Time}, {_Type, Pid, _}}) ->
    NameStr = lists:flatten(io_lib:format("~w", [Pid])),
    {NameStr, date_str(Date,Time)};
get_short_descr(_) ->
    {"???", "???"}.
    
date_str({Y,Mo,D}=Date,{H,Mi,S}=Time) ->
    case application:get_env(sasl,utc_log) of 
	{ok,true} ->
	    {{YY,MoMo,DD},{HH,MiMi,SS}} = 
		local_time_to_universal_time({Date,Time}),
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w UTC", 
					[YY,MoMo,DD,HH,MiMi,SS]));
	_ ->
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w", 
					[Y,Mo,D,H,Mi,S]))
    end.

local_time_to_universal_time({Date,Time}) ->
    case calendar:local_time_to_universal_time_dst({Date,Time}) of
	[UCT] ->
	    UCT;
	[UCT1,_UCT2] ->
	    UCT1;
	[] -> % should not happen
	    {Date,Time}
    end.

read_rep_msg(Fd, FilePosition) ->
    file:position(Fd, {bof, FilePosition}),
    Res = 
		case catch read_report(Fd) of
		    {ok, Report} ->
				{_ShortDescr, Date} = get_short_descr(Report),
				{Date, Report};
		    _ -> error
		end,
    file:close(Fd),
    Res.