-record(configuration, {
    network :: atom(),
    hostname :: string(),
    port :: integer()
}).

-record(state, {
    continuation :: binary(),
    configuration :: #configuration{},
    socket :: inet:socket(),
    ping_timer :: timer:tref(),
    pong_timestamp :: calendar:datetime(),
    ircd_handler :: atom()
}).

