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
	start_webtool/0, start_webtool/3, total_writes/0,
	discover/0, reload/0, build_rel/0
	]).
	
%%%
%%% Application API
%%%

%% @doc start the application
start(_StartType, _StartArgs) -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
%% @doc stop the application
stop(_) -> 
	ok.
		
start_webtool() ->
	start_webtool(standard_data).

start_webtool(Args) ->
	webtool:start(standard_path, Args),
	webtool:start_tools([],"app=log_roller"),
	ok.
		
start_webtool(Port, BindAddr, ServerName) when is_integer(Port), is_tuple(BindAddr), is_list(ServerName) ->
	io:format("start_webtool(~p, ~p, ~p)~n", [Port, BindAddr, ServerName]),
	(catch net_adm:world()),
	start_webtool([{port,Port},{bind_address,BindAddr},{server_name,ServerName}]),
	ok;

start_webtool(Port, BindAddr, ServerName) ->
	Port1 = 
		case Port of
			P when is_integer(P) -> P;
			P when is_atom(P) -> list_to_integer(atom_to_list(P));
			P when is_list(P) -> list_to_integer(P)
		end,
	Fun = 
		fun(Addr) ->
			Vals = string:tokens(re:replace(Addr,"[\{\}]","", [global, {return, list}]), ".,"),
			list_to_tuple([list_to_integer(Val) || Val <- Vals])
		end,
	BindAddr1 =
		case BindAddr of
			B when is_atom(B) -> Fun(atom_to_list(B));
			B when is_list(B) -> Fun(B);
			B when is_tuple(B) -> B
		end,
	ServerName1 =
		case ServerName of
			S when is_atom(S) -> atom_to_list(S);
			S when is_list(S) -> S
		end,
	start_webtool(Port1, BindAddr1, ServerName1).
	
total_writes() ->
	log_roller_disk_logger:total_writes().
	
%%%
%%% Internal functions
%%%

%% @hidden
init(_) ->
	{ok, {{one_for_one, 10, 10}, [
	    {log_roller_disk_logger, {log_roller_disk_logger, start_link, []}, permanent, 5000, worker, [log_roller_disk_logger]},
		{log_roller_browser, {log_roller_browser, start_link, []}, permanent, 5000, worker, [log_roller_browser]}
	]}}.

%% @hidden
start_phase(world, _, _) ->
	net_adm:world(),
	ok;

%% @hidden
start_phase(discovery, _, _) ->
	spawn_link(fun discover/0),
	ok.
	
discover() ->
	[log_roller_disk_logger:subscribe_to(Node) || Node <- [node()|nodes()]],
	timer:sleep(360000),
	discover().

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
	
	