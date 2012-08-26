-module(ircd_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_message, 2}];
behaviour_info(_Other) ->
    undefined.
