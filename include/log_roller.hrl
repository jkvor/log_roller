-define(WEBTOOL_ARGS, [standard_path, [{port,4057},{bind_address,{0,0,0,0}},{server_name,"localhost"}]]).
-define(MIN_BYTES, 104856).
-define(NUM_TABLES, 10).

-record(counter, {key, value}).
-record(log_roller_config, {key, value}).
-record(log_entry, {id, type, node, time, message}).
