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
-module(log_roller_test).
-compile(export_all).

setup_test(MaxBytes, MaxFiles) ->
	ok = error_logger:tty(false),
	
	etap_exception:lives_ok(fun() ->
		etap_application:load_ok(log_roller_server, "Application 'log_roller_server' loaded"),
		etap_application:load_ok(log_roller, "Application 'log_roller' loaded"),
		ok
	end, "load log roller"),
	
	Log_Dir = rnd_dir(),
	put(log_dir, Log_Dir),
	
	application:set_env(log_roller_server, log_dir, Log_Dir),
	application:set_env(log_roller_server, maxbytes, MaxBytes),
	application:set_env(log_roller_server, maxfiles, MaxFiles),
	
	etap_exception:lives_ok(fun() ->
		etap_application:start_ok(log_roller_server, "Application 'log_roller_server' started"),
		etap_application:start_ok(log_roller, "Application 'log_roller' started"),
		ok
	end, "start log roller"),
	
	ok.
	
teardown_test() ->
	etap:is(rm_dir(get(log_dir)), ok, "remove temp log directory"),
	ok.
	
rnd_dir() ->
	{A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
	Dir = "." ++ lists:flatten([
        [[random:uniform(25) + 96] || _ <-lists:seq(1,5)],
        [[random:uniform(9) + 47] || _ <-lists:seq(1,3)]
    ]),
	io:format("temp log directory: ~p~n", [Dir]),
	Dir.

rm_dir(Dir) ->
	{ok, Filenames} = file:list_dir(Dir),
	[file:delete(Dir ++ "/" ++ File) || File <- Filenames],
	file:del_dir(Dir).