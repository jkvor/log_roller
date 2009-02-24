{application, log_roller_subscriber, [
    {description, "log_roller_subscriber"},
    {vsn, "0.0.1"},
    {modules, [
        log_roller_subscriber,
		log_roller_disk_logger,
		log_roller_browser,
		log_roller_webtool,
		lrb,
		rb_raw
    ]},
    {registered, []},
    {mod, {log_roller_subscriber, []}},
    {applications, [kernel, stdlib]},
	{start_phases, [{world, []}, {discovery, []}]}
]}.
