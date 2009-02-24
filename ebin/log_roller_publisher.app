{application, log_roller_publisher, [
    {description, "log_roller_publisher"},
    {vsn, "0.0.1"},
    {modules, [
        log_roller_publisher,
		log_roller_h
    ]},
    {registered, []},
    {mod, {log_roller_publisher, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}, {discovery, []}]}
]}.
