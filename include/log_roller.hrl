-define(MIN_BYTES, 104856).
-define(NUM_TABLES, 10).

-record(counter, {key, value}).
-record(log_roller_config, {key, value}).
-record(log_entry, {id, type, node, time, message}).
