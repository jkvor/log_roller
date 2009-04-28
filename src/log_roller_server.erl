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
-module(log_roller_server).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([
	start/2, stop/1, init/1, start_phase/3, 
	total_writes/0, determine_subscriptions/0, 
	compile_templates/0, reload/0, build_rel/0
]).

-include("log_roller.hrl").
	
%%%
%%% Application API
%%%

%% @doc start the application
start(_StartType, _StartArgs) -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
%% @doc stop the application
stop(_) -> 
	ok.
	
compile_templates() ->
  {ok, Filenames} = file:list_dir("templates"),
  [erltl:compile("templates/" ++ Filename, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars]) 
    || Filename <- Filenames],
  ok.
  		
total_writes() ->
	log_roller_disk_logger:total_writes().
	
%%%
%%% Internal functions
%%%

%% @hidden
init(_) ->
	DiskLoggers = get_disk_loggers_from_config(),
	DiskLoggerChildren =
		[begin
			{?Server_Name(DiskLogger#disk_logger.name), {log_roller_disk_logger, start_link, [DiskLogger]}, permanent, 5000, worker, [log_roller_disk_logger]}
		 end || DiskLogger <- DiskLoggers],
	Lrb = {lrb, {lrb, start_link, [DiskLoggers]}, permanent, 5000, worker, [lrb]}, 
	Lrws = {log_roller_web_server, {log_roller_web_server, start_link, [[]]}, permanent, 5000, worker, [log_roller_web_server]},
	{ok, {{one_for_one, 10, 10}, 
		lists:reverse([Lrb, Lrws | DiskLoggerChildren])
	}}.
	
%% {disk_logger, Name::atom(), Nodes::[node()], Filters::[{atom(), any()}], LogDir::string(), MaxBytes::integer(), MaxFiles::integer()}
get_disk_loggers_from_config() ->
	case proplists:delete(included_applications, application:get_all_env(log_roller_server)) of
		[] ->
			[#disk_logger{}];
		Envs ->
			get_disk_loggers_from_config(Envs, [])
	end.

get_disk_loggers_from_config([], Loggers) -> Loggers;
	
get_disk_loggers_from_config([{log_dir, LogDir}|Tail], Loggers) when is_list(LogDir) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{log_dir=LogDir}|Loggers1],
	get_disk_loggers_from_config(Tail, Loggers2);

get_disk_loggers_from_config([{filters, Filters}|Tail], Loggers) when is_list(Filters) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{filters=Filters}|Loggers1],
	get_disk_loggers_from_config(Tail, Loggers2);
		
get_disk_loggers_from_config([{cache_size, CacheSize}|Tail], Loggers) when is_integer(CacheSize) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{cache_size=CacheSize}|Loggers1],
	get_disk_loggers_from_config(Tail, Loggers2);
		
get_disk_loggers_from_config([{maxbytes, Maxbytes}|Tail], Loggers) when is_integer(Maxbytes) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{maxbytes=Maxbytes}|Loggers1],
	get_disk_loggers_from_config(Tail, Loggers2);
	
get_disk_loggers_from_config([{maxfiles, Maxfiles}|Tail], Loggers) when is_integer(Maxfiles) ->
	{Logger, Loggers1} = get_default_logger(Loggers),
	Loggers2 = [Logger#disk_logger{maxfiles=Maxfiles}|Loggers1],
	get_disk_loggers_from_config(Tail, Loggers2);
	
get_disk_loggers_from_config([{Custom, Args}|Tail], Loggers) when is_atom(Custom), is_list(Args) ->	
	Fields = record_info(fields, disk_logger),
	Index = [I+1 || I <- lists:seq(1, length(Fields))],
	Ref = lists:zip(Fields, Index),
	Logger = populate_disk_logger_fields(#disk_logger{name=Custom}, [filters, log_dir, cache_size, maxbytes, maxfiles], Args, Ref),
	get_disk_loggers_from_config(Tail, [Logger|Loggers]).

get_default_logger(Loggers) ->
	case lists:keytake(default, 2, Loggers) of
		{value, Default, Loggers1} ->
			{Default, Loggers1};
		false ->
			{#disk_logger{}, Loggers}
	end.

populate_disk_logger_fields(Logger, [], _, _) -> Logger;

populate_disk_logger_fields(Logger, [Field|Tail], Args, Ref) ->
	case proplists:get_value(Field, Args) of
		undefined ->
			populate_disk_logger_fields(Logger, Tail, Args, Ref);
		Value ->
			Index = proplists:get_value(Field, Ref),
			Logger1 = erlang:setelement(Index, Logger, Value),
			populate_disk_logger_fields(Logger1, Tail, Args, Ref)
	end.

%% @hidden
start_phase(pg2, _, _) ->
    pg2:which_groups(),
    ok;
        
%% @hidden
start_phase(world, _, _) ->
	net_adm:world(),
	ok;
	
%% @hidden
start_phase(discovery, _, _) ->
	Subscriptions = determine_subscriptions(),
	spawn(fun() -> register_as_subscriber(Subscriptions) end),
	ok.

determine_subscriptions() ->
    determine_subscriptions(get_disk_loggers_from_config(), [node()|nodes()], []).
    
%% [{disk_logger(), [node()]}]
determine_subscriptions([], _, Acc) -> Acc;

determine_subscriptions([DiskLogger|Tail], Nodes, Acc) when is_record(DiskLogger, disk_logger), is_list(Nodes) ->
	Acc1 =
		case proplists:get_value(nodes, DiskLogger#disk_logger.filters, []) of
			[] ->
				[{DiskLogger, Nodes}|Acc];
			_ ->
				case get_matching_nodes(DiskLogger, Nodes) of
					[] ->
						Acc;
					Matches ->
						[{DiskLogger, Matches}|Acc]
				end
		end,
	determine_subscriptions(Tail, Nodes, Acc1).	
		
get_matching_nodes(DiskLogger, Nodes) ->
	DiskLoggerNodes = proplists:get_value(nodes, DiskLogger#disk_logger.filters),
	lists:filter(fun(Node) -> lists:member(Node, DiskLoggerNodes) end, Nodes).

%% send a subscribe message to all connected nodes
register_as_subscriber(Subscriptions) ->
	[begin
		[begin
			log_roller_disk_logger:register_as_subscriber_with(DiskLogger#disk_logger.name, Node)
		 end || Node <- Nodes]
	 end || {DiskLogger, Nodes} <- Subscriptions],	
	timer:sleep(360000),
	register_as_subscriber(Subscriptions).

reload() ->
	{ok, Modules} = application_controller:get_key(?MODULE, modules),
	[begin
		case code:is_loaded(Module) of
        	false -> ok;
        	{file, _Path} ->
            	code:purge(Module),
            	code:load_file(Module)
    	end 
 	 end || Module <- Modules],
	log_roller_disk_logger:reload().

build_rel() ->
	{ok, FD} = file:open("bin/log_roller_server.rel", [write]),
	RootDir = code:root_dir(),
	Patterns = [
	    {RootDir ++ "/", "erts-*"},
	    {RootDir ++ "/lib/", "kernel-*"},
	    {RootDir ++ "/lib/", "stdlib-*"}
	],
	[Erts, Kerne, Stdl] = [begin
	    [R | _ ] = filelib:wildcard(P, D),
	    [_ | [Ra] ] = string:tokens(R, "-"),
	    Ra
	end || {D, P} <- Patterns],
	RelInfo = {release,
	    {"log_roller_server", "0.2"},
	    {erts, Erts}, [
	        {kernel, Kerne},
	        {stdlib, Stdl},
	        {log_roller_server, "0.2"}
	    ]
	},
	io:format(FD, "~p.", [RelInfo]),
	file:close(FD),
	systools:make_script("bin/log_roller_server", [local]),
	ok.
	
	