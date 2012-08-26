-record(config, {
    network :: atom(),
    hostname :: string(),
    port :: integer(),
    transport :: atom(),
    options :: [any()]
}).
