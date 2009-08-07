{application, log_roller_server, [
    {description, "log_roller_server"},
    {vsn, "0.3"},
    {modules, [
        log_roller_server,
		log_roller_disk_logger,
		log_roller_cache,
		log_roller_filter,
		lrb,
		log_roller_web_server,
		log_roller_tail,
		log_roller_utils
    ]},
    {registered, []},
    {mod, {log_roller_server, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{pg2, []}, {world, []}, {discovery, []}]}
]}.
