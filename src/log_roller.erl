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
-module(log_roller).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([
	start/0, start/2, stop/1, start_phase/3, queue_length/0,
	register_subscriber/1, get_registered_subscribers/0
]).

%%%
%%% Application API
%%%
start() ->
    application:start(?MODULE).

%% @spec start(StartType, StartArgs) -> {ok, Pid}
%% @doc start the application
start(_StartType, _StartArgs) -> 
	ok = error_logger:add_report_handler(log_roller_h),
	{ok, self()}.
	
%% @doc stop the application
stop(_) ->
	ok.

get_registered_subscribers() ->
	gen_event:call(error_logger, log_roller_h, subscribers).
	
queue_length() ->
	erlang:process_info(erlang:whereis(error_logger), message_queue_len).
		
%%%
%%% Internal functions
%%%

%% @hidden
start_phase(world, _, _) ->
	(catch net_adm:world()),
	ok;

%% @hidden
start_phase(discovery, _, _) ->
	[register_subscriber(Node) || Node <- [node()|nodes()]],
	ok.
	
%% @spec register_subscriber(Node::node()) -> ok
%% @doc ping Node to determine if it is a subscriber and register with event handler if it is
register_subscriber(Node) when is_atom(Node) ->
	case catch rpc:call(Node, log_roller_disk_logger, ping, [node()]) of
		List when is_list(List) ->
			[begin
				gen_event:call(error_logger, log_roller_h, {subscribe, Pid})
			 end || {ok, Pid} <- List];
		{badrpc,{'EXIT',{noproc,{gen_server,call,[log_roller_disk_logger,ping]}}}} ->
			ok;
		{error, application_not_running} ->
			ok;
		_Err ->
			ok
	end.