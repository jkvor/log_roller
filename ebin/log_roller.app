{application, log_roller, [
    {description, "log_roller"},
    {vsn, "0.0.1"},
    {modules, [
        log_roller,
		log_roller_h,
		log_roller_subscriber
    ]},
    {registered, []},
    {mod, {log_roller, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}]}
]}.