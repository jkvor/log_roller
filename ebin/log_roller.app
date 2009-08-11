{application, log_roller, [
    {description, "log_roller"},
    {vsn, "0.3"},
    {modules, [
        log_roller,
		log_roller_h,
		log_roller_logger
    ]},
    {registered, []},
    {mod, {log_roller, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}, {discovery, []}]}
]}.
