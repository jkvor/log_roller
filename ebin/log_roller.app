{application, log_roller, [
    {description, "log_roller"},
    {vsn, "0.2"},
    {modules, [
        log_roller,
		log_roller_h
    ]},
    {registered, []},
    {mod, {log_roller, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}, {discovery, []}]}
]}.
