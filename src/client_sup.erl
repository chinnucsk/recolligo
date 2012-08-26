-module(client_sup).
-export([start/0, init/1]).
-behaviour(supervisor).

-include("configuration.hrl").

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 60}, []}}.
