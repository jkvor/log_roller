-module(lrb).
-author('jacob.vorreuter@gmail.com').

-export([list/0, list/1, show/0, show/1]).
-export([create_counter_table/0, create_config_table/0, lookup_log_index/0, switch_log_tables/1, next_index/1, table_name/1]).
-export([select_all/1]).

-include("log_roller.hrl").

-define(DEFAULT_MAX, 100).
-define(DEFAULT_TYPE, all).

list() -> list([]).
	
%% @spec list(Args) -> Result
%%		 Args = [{max,_}, {type,_}, {node,_}, {time,_}, {grep,_}]
%%		 Result = [{type, node, time, message}]
list(Args) ->
	case proplists:is_defined(grep, Args) of
		true ->
			format_list(filter(Args));
		false ->
			format_list(fetch(Args))
	end.
	
show() -> show([]).

show(ID) when is_integer(ID) ->
	format_show(fetch([{max, 1}, {id, ID}]));
	
show(Args) when is_list(Args) ->
	case proplists:is_defined(grep, Args) of
		true ->
			format_show(filter(Args));
		false ->
			format_show(fetch(Args))
	end.
	
fetch(Args) ->
	Index = lookup_log_index(),
	{Max, Args1} =
		case lists:keytake(max, 1, Args) of
			{value, {max, Max0}, Args0} -> {Max0, Args0};
			false -> {0, Args}
		end,
	fetch(Max, Args1, [], Index, next_index(Index)).
	
fetch(Max, _, Acc, _, _) when Max > 0, length(Acc) >= Max -> 
	lists:sublist(Acc, Max);

fetch(_, _, Acc, Index, Index) -> 
	Acc;

fetch(Max, Args, Acc, Index, Final) ->
	TableName = table_name(Index),
	case table_exists(TableName) of
		true ->
			Res = match_object(Args, TableName),
			fetch(Max, Args, lists:append(Res, Acc), prev_index(Index), Final);
		false ->
			fetch(Max, Args, Acc, Final, Final)
	end.

filter(Args) ->
	Index = lookup_log_index(),
	{Max, Args1} =
		case lists:keytake(max, 1, Args) of
			{value, {max, Max0}, Args0} -> {Max0, Args0};
			false -> {0, Args}
		end,
	{ok, Regexp} = re:compile(proplists:get_value(grep, Args1)),
	filter(Regexp, Max, Args1, [], Index, next_index(Index)).
		
filter(_, Max, _, Acc, _, _) when Max > 0, length(Acc) >= Max -> 
	lists:sublist(Acc, Max);
		
filter(_, _, _, Acc, Index, Index) -> 
	Acc;
	
filter(RE, Max, Args, Acc, Index, Final) ->
	TableName = table_name(Index),
	case table_exists(TableName) of
		true ->
			Res = match_object(Args, TableName),
			Res1 = lists:foldr(
					fun(Log, Acc0) ->
						case re:run(flatten(Log#log_entry.message), RE) of
							{match, _} -> [Log|Acc0];
							nomatch -> Acc0
						end
					end, [], Res),
			filter(RE, Max, Args, lists:append(Res1, Acc), prev_index(Index), Final);
		false ->
			filter(RE, Max, Args, Acc, Final, Final)
	end.
	
match_object(Args, TableName) ->
	Type = 
		case proplists:get_value(type, Args, all) of
			all -> '_';
			Type0 -> Type0
		end,
	Log = #log_entry{
		id=proplists:get_value(id, Args, '_'), 
		type=Type, 
		node=proplists:get_value(node, Args, '_'), 
		time='_', 
		message='_'
	},
	mnesia:dirty_match_object(TableName, Log).
	
create_config_table() ->
	case table_exists(log_roller_config) of
		true -> ok;
		false ->
			{atomic, ok} = mnesia:create_table(log_roller_config,
								[{disc_copies, [node()]},
					 		 	 {attributes, record_info(fields, log_roller_config)}])
	end, ok.
	
create_counter_table() ->
	case table_exists(counter) of
		true -> ok;
		false ->
			{atomic, ok} = mnesia:create_table(counter, 
	                        	[{disc_copies, [node()]}, 
	                         	 {attributes, record_info(fields, counter)}])
	end, ok.
			
switch_log_tables(Index) when is_integer(Index) ->	
	TableName = table_name(Index),
	case table_exists(TableName) of
		true ->
			{atomic, ok} = mnesia:clear_table(TableName);
		false ->
			{atomic, ok} = mnesia:create_table(TableName, 
									[{disc_copies, [node()]},
								      {type, ordered_set},
								      {local_content, true},
								      {record_name, log_entry},
								      {attributes, record_info(fields, log_entry)}])
	end,
	mnesia:dirty_write({log_roller_config, index, Index}).
	
lookup_log_index() ->
	case mnesia:dirty_read(log_roller_config, index) of
		[] -> 
			ok = switch_log_tables(1),
			1;
		[{log_roller_config, index, Index}] -> 
			Index
	end.
	
select_all(Index) ->
	mnesia:dirty_match_object(table_name(Index), {log_entry, '_', '_', '_', '_', '_'}).
	
table_name(Index) when is_integer(Index) -> 
	list_to_atom("log_roller_" ++ integer_to_list(Index)).

table_exists(TableName) when is_atom(TableName) ->
	lists:member(TableName, mnesia:system_info(local_tables)).
	
next_index(CurrentIndex) when is_integer(CurrentIndex), CurrentIndex >= ?NUM_TABLES -> 1;
next_index(CurrentIndex) when is_integer(CurrentIndex) -> CurrentIndex + 1.

prev_index(CurrentIndex) when is_integer(CurrentIndex), CurrentIndex =< 1 -> ?NUM_TABLES;
prev_index(CurrentIndex) when is_integer(CurrentIndex) -> CurrentIndex - 1.

format_list(List) -> 
	[begin
		{ID, Type, Node, format_time(Time)}
	 end || #log_entry{id=ID, type=Type, node=Node, time=Time} <- List].

format_show(List) -> 
	[begin
		{ID, Type, Node, format_time(Time), Msg}
	 end || #log_entry{id=ID, type=Type, node=Node, time=Time, message=Msg} <- List].
	
format_time({{Y,Mo,D},{H,Mi,S}}) ->
	lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [Y,Mo,D,H,Mi,S])).
	
flatten(Stuff) -> lists:flatten(io_lib:format("~p", [Stuff])).