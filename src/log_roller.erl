-module(log_roller).
-author('jacob.vorreuter@gmail.com').
-behaviour(application).

-export([start/2, stop/1, start_webtool/0, start_webtool/3]).
-export([build_rel/0, compile_templates/0, reload/0]).

-include("log_roller.hrl").

start(_StartType, StartArgs) -> 
	ok = error_logger:add_report_handler(log_roller_h, StartArgs),
	{ok, self()}.

stop(_) -> ok.

start_webtool() -> start_webtool(8888, {0,0,0,0}, "localhost").
start_webtool(Port, BindAddr, ServerName) ->
	webtool:start(standard_path, [{port,Port},{bind_address,BindAddr},{server_name,ServerName}]),
	webtool:start_tools([],"app=log_roller"),
	ok.
	
build_rel() ->
    {ok, FD} = file:open("log_roller.rel", [write]),
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
        {"log_roller", "0.0.1"},
        {erts, Erts}, [
            {kernel, Kerne},
            {stdlib, Stdl},
            {log_roller, "0.0.1"}
        ]
    },
    io:format(FD, "~p.", [RelInfo]),
    file:close(FD),
    systools:make_script("log_roller", [local]),
    ok.

compile_templates() ->
	case file:list_dir("templates") of
		{ok, Filenames} ->
			[begin
				erltl:compile("templates/" ++ Filename, [
								{outdir, "ebin"},
					       		report_errors, report_warnings, nowarn_unused_vars])
			 end || Filename <- Filenames];
		_ -> 
			exit(failure)
	end.

reload() ->
    Modules = [log_roller, log_roller_h],
    lists:foreach(
        fun(Module) ->
            case code:is_loaded(Module) of
                false -> ok;
                {file, _Path} ->
                    code:purge(Module),
                    code:load_file(Module)
            end
        end,
        Modules
    ).