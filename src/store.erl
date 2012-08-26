-module(store).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([connecting_client/2, disconnecting_client/2]).
-behaviour(gen_server).

-record(state, { redis :: pid() }).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

connecting_client(Network, Hostmask) ->
    gen_server:cast(?MODULE, {connecting_client, Network, Hostmask}).

disconnecting_client(Network, Hostmask) ->
    gen_server:cast(?MODULE, {disconnecting_client, Network, Hostmask}).

init([]) ->
    {ok, Pid} = eredis:start_link(),
    {ok, #state { redis = Pid }}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({connecting_client, Network, Hostmask}, State) ->
    Pid = State#state.redis,
    Store = "clients:" ++ atom_to_list(Network),
    {ok, <<"OK">>} = eredis:q(Pid, ["MULTI"]),
    {ok, <<"QUEUED">>} = eredis:q(Pid, ["SADD", Store, Hostmask]),
    {ok, <<"QUEUED">>} = eredis:q(Pid, ["PUBLISH", Store ++ ":connecting", Hostmask]),
    {ok, [_, _]} = eredis:q(Pid, ["EXEC"]),
    {noreply, State};
handle_cast({disconnecting_client, Network, Hostmask}, State) ->
    Pid = State#state.redis,
    Store = "clients:" ++ atom_to_list(Network),
    {ok, <<"OK">>} = eredis:q(Pid, ["MULTI"]),
    {ok, <<"QUEUED">>} = eredis:q(Pid, ["SREM", Store, Hostmask]),
    {ok, <<"QUEUED">>} = eredis:q(Pid, ["PUBLISH", Store ++ ":disconnecting", Hostmask]),
    {ok, [_, _]} = eredis:q(Pid, ["EXEC"]),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Reason, State, _Extra) ->
    {ok, State}.
