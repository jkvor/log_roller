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
-module(log_roller_subscriber).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([start/2, stop/1, init/1, start_phase/3, start_webtool/0, reload/0]).
	
%%%
%%% Application API
%%%

%% @doc start the application
start(_StartType, _StartArgs) -> 
	case init:get_argument(webtool) of 
    	{ok,[["start"]]} ->
			start_webtool();
		_ ->
			ok
	end,
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
%% @doc stop the application
stop(_) -> 
	ok.

start_webtool() -> 
	Port = 
		case application:get_env(log_roller_subscriber, webtool_port) of
			undefined -> 8888;
			{ok, Val1} -> Val1
		end,
	BindAddr = 
		case application:get_env(log_roller_subscriber, webtool_bindaddr) of
			undefined -> {0,0,0,0};
			{ok, Val2} -> Val2
		end,
	Server = 
		case application:get_env(log_roller_subscriber, webtool_server) of
			undefined -> "localhost";
			{ok, Val3} -> Val3
		end,
	start_webtool(Port, BindAddr, Server).
		
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
	[log_roller_disk_logger:subscribe_to(Node) || Node <- [node()|nodes()]],
	ok.
		
%% @hidden	
start_webtool(Port, BindAddr, ServerName) ->
	webtool:start(standard_path, [{port,Port},{bind_address,BindAddr},{server_name,ServerName}]),
	webtool:start_tools([],"app=log_roller"),
	ok.

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
	
	
	