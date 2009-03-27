{application, log_roller_server, [
    {description, "log_roller_server"},
    {vsn, "0.2"},
    {modules, [
        log_roller_server,
		log_roller_disk_logger,
		lrb,
		log_roller_webtool
    ]},
    {registered, []},
    {mod, {log_roller_server, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}, {discovery, []}]}
]}.
