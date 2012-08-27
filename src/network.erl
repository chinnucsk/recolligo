-module(network).
-export([start/1, init/1, start_server/1]).
-behaviour(supervisor).

-include("config.hrl").

start(Network) ->
    supervisor:start_link({local, Network}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 600}, [{client, {client, start, []}, transient, 5000, worker, [client]}]}}.

start_server(#config{ network = Network } = Config) ->
    supervisor:start_child(Network, [Config]).
