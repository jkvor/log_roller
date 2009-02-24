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
-module(log_roller_publisher).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([start/2, stop/1, start_phase/3]).
	
%%%
%%% Application API
%%%

%% @doc start the application
start(_StartType, _StartArgs) -> 
	ok = error_logger:add_report_handler(log_roller_h, []),
	{ok, self()}.
	
%% @doc stop the application
stop(_) -> 
	ok.
	
%%%
%%% Internal functions
%%%

%% @hidden
start_phase(world, _, _) ->
	net_adm:world(),
	ok;

%% @hidden
start_phase(discovery, _, _) ->
	[log_roller_h:register_subscriber(Node) || Node <- nodes()],
	ok.
