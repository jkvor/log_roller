{application, log_roller_server, [
    {description, "log_roller_server"},
    {vsn, "0.2"},
    {modules, [
        log_roller_server,
		log_roller_disk_logger,
		log_roller_cache,
		log_roller_filter,
		lrb,
		log_roller_webtool
    ]},
    {registered, []},
    {mod, {log_roller_server, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{pg2, []}, {world, []}, {discovery, []}]}
]}.
