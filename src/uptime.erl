-module(uptime).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([uptime/0, relative_uptime/0]).
-behaviour(gen_server).

-record(state, { timestamp :: calendar:datetime() }).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

uptime() ->
    gen_server:call(?MODULE, uptime).

relative_uptime() ->
    gen_server:call(?MODULE, relative_uptime).

init([]) ->
    {ok, #state { timestamp = erlang:localtime() }}.

handle_call(uptime, _From, State) ->
    Delta = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(State#state.timestamp),
    {reply, Delta, State};
handle_call(relative_uptime, _From, State) ->
    Delta = utilities:relative_time_difference(State#state.timestamp, erlang:localtime()),
    {reply, Delta, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Reason, State, _Extra) ->
    {ok, State}.
