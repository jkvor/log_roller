-module(lrb).
-author('jacob.vorreuter@gmail.com').

-export([list/0, list/1, show/0, show/1]).
-export([create_counter_table/0, create_config_table/0, lookup_log_index/0, switch_log_tables/1, next_index/1, table_name/1]).

-include("log_roller.hrl").

-define(DEFAULT_MAX, 100).
-define(DEFAULT_TYPE, all).

list() -> list([]).
	
%% @spec list(Args) -> Result
%%		 Args = [{max,_}, {type,_}, {node,_}, {time,_}, {grep,_}]
%%		 Result = [{type, node, time, message}]
list(Args) ->
	Max = proplists:get_value(max, Args, ?DEFAULT_MAX),
	Type = proplists:get_value(type, Args, ?DEFAULT_TYPE),
	_Grep = proplists:get_value(grep, Args, undefined),
	format_list(fetch(Max, Type)).
	
show() -> show([]).

show(Args) ->
	Max = proplists:get_value(max, Args, ?DEFAULT_MAX),
	Type = proplists:get_value(type, Args, ?DEFAULT_TYPE),
	_Grep = proplists:get_value(grep, Args, undefined),
	format_show(fetch(Max, Type)).
	
fetch(Max, Type) ->
	Index = lookup_log_index(),
	fetch(Max, Type, [], Index, next_index(Index)).
	
fetch(Max, _, Acc, _, _) when length(Acc) >= Max -> 
	lists:sublist(Acc, Max);

fetch(_, _, Acc, Index, Index) -> 
	Acc;

fetch(Max, Type, Acc, Index, Final) ->
	TableName = table_name(Index),
	case table_exists(TableName) of
		true ->
			Res =
				case Type of
					all ->
						mnesia:dirty_match_object(TableName, #log_entry{id='_', type='_', node='_', time='_', message='_'});
					_ ->
						mnesia:dirty_match_object(TableName, #log_entry{id='_', type=Type, node='_', time='_', message='_'})
				end, 
			fetch(Max, Type, lists:append(Res, Acc), prev_index(Index), Final);
		false ->
			fetch(Max, Type, Acc, Final, Final)
	end.

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