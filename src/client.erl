-module(client).
-export([start/1, stop/2]).
-export([quit/3, ping/2, send/2, config/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-include("protocol.hrl").
-include("config.hrl").

-record(state, {
    config :: #config{},
    continuation :: binary(),
    socket :: inet:socket(),
    ping_timer :: timer:tref(),
    pong_timestamp :: calendar:datetime()
}).

config(#state { config = Config }) ->
    Config.

atomify(Network, Hostname) ->
    list_to_atom(atom_to_list(Network) ++ "/" ++ Hostname).

start(#config{ network = Network, hostname = Hostname } = Config) ->
    gen_server:start_link({local, atomify(Network, Hostname)}, ?MODULE, Config, []).

stop(Network, Hostname) when is_list(Hostname) ->
    gen_server:call(atomify(Network, Hostname), stop).

quit(Network, Hostname, Message) ->
    gen_server:call(atomify(Network, Hostname), {quit, Message}).

ping(Network, Hostname) ->
    gen_server:cast(atomify(Network, Hostname), ping).

init(Config) ->
    Transport = config:transport(Config),
    case Transport:connect(config:hostname(Config), config:port(Config), [binary, {packet, 0}, {active, once}]) of
        {ok, Socket} ->
            State = #state{ continuation = <<>>,
                            config = Config,
                            socket = Socket,
                            ping_timer = undefined,
                            pong_timestamp = erlang:localtime() },
            send(State, {nick, config:nickname(Config)}),
            send(State, {user, config:username(Config), "XXX", "XXX", config:realname(Config)}),
            {ok, start_ping_timer(State)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({quit, Message}, _From, State) ->
    send(State, {quit, Message}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(ping, State) ->
    case check_time_difference(State) of
        ok ->
            send(State, {ping, config:hostname(config(State))}),
            {noreply, reset_ping_timer(State)};
        Error ->
            % We should quit with an error message a la: "No data from server:
            % ~i".
            {stop, Error, State}
    end;
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({ssl, Socket, Packet}, #state { socket = Socket } = State) ->
    handle_packet(Packet, State);
handle_info({ssl_closed, Socket}, #state { socket = Socket} = State) ->
    {stop, closed, State};
handle_info({ssl_error, Socket, Reason}, #state { socket = Socket} = State) ->
    {stop, Reason, State};
handle_info({tcp, Socket, Packet}, #state { socket = Socket } = State) ->
    handle_packet(Packet, State);
handle_info({tcp_closed, Socket}, #state { socket = Socket } = State) ->
    {stop, closed, State};
handle_info({tcp_error, Socket, Reason}, #state { socket = Socket} = State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Config = config(State),
    Transport = config:transport(Config),
    Transport:close(State#state.socket),
    stop_ping_timer(State),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

ack_socket(State) ->
    Config = config(State),
    Transport = config:transport(Config),
    Transport:setopts(State#state.socket, [{active, once}]).

handle_packet(Chunk, #state { continuation = Cont } = State) ->
    ack_socket(State),
    try
        case process_packet(Chunk, Cont) of
            {messages, Messages, NewCont} ->
                {noreply, lists:foldl(fun handle_line/2, State#state{ continuation = NewCont }, Messages)};
            {ok, NewCont} ->
                {noreply, State#state{ continuation = NewCont }}
        end
    catch
        throw:Error ->
            {stop, Error, State}
    end.

process_packet(Chunk, Continuation) ->
    process_packet(Chunk, Continuation, []).

process_packet(Chunk, Continuation, Messages) ->
    Data = <<Continuation/binary, Chunk/binary>>,
    case binary:split(Data, <<"\r\n">>, []) of
        [Line, Rest] ->
            process_packet(<<>>, Rest, [Line | Messages]);
        [_] ->
            return_messages(Messages, Data)
    end.

return_messages([], Data) ->
    {ok, Data};
return_messages(Messages, Data) ->
    {messages, lists:reverse(Messages), Data}.

handle_line(Line, State) ->
    NewState = reset_ping_timer(State#state { pong_timestamp = erlang:localtime() }),
    Config = config(State),
    IrcdHandler = config:ircd_handler(Config),
    case protocol:parse(Line) of
        {ok, Message} ->
            NewState2 = handle_message(NewState, Message),
            IrcdHandler:handle_message(NewState2, Message);
        {error, _} ->
            NewState
    end.

handle_message(State, Message) ->
    Config = config(State),
    io:format("(~s/~s) -> ~s~n", [atom_to_list(config:network(Config)), config:hostname(Config), protocol:render(Message)]),
    handle_one_message(Message#message.prefix, Message#message.command, Message#message.arguments, State).

handle_one_message(<<>>, ping, [Argument], State) ->
    send(State, {pong, Argument});
handle_one_message(_Hostname, '433', [_, Nickname, _], State) ->
    send(State, {nick, binary_to_list(Nickname) ++ "_"});
handle_one_message(Hostmask, privmsg, Message, State) ->
    handle_one_privmsg(Hostmask, Message, State);
handle_one_message(_Prefix, _Command, _Arguments, State) ->
    State.

handle_one_privmsg(Hostmask, [_, <<"@", X/binary>>], State) ->
    case binary:split(X, <<" ">>, [global]) of
        [Command | Arguments] ->
            command_handler:handle(Hostmask, binary_to_atom(Command, utf8), lists:map(fun binary_to_list/1, Arguments), State);
        _Otherwise ->
            State
    end;
handle_one_privmsg(_Hostmask, _Arguments, State) ->
    State.

send(State, Message) ->
    Config = config(State),
    Transport = config:transport(Config),
    Text = irc:render(Message),
    io:format("(~s/~s) <- ~s~n", [atom_to_list(config:network(Config)), config:hostname(Config), Text]),
    Transport:send(State#state.socket, [Text, "\r\n"]),
    State.

start_ping_timer(State) ->
    Config = config(State),
    {ok, PingTimer} = timer:apply_after(config:ping_interval(Config) * 1000, client, ping, [config:network(Config), config:hostname(Config)]),
    State#state{ ping_timer = PingTimer }.

stop_ping_timer(#state { ping_timer = PingTimer } = State) ->
    timer:cancel(PingTimer),
    State#state{ ping_timer = undefined }.

reset_ping_timer(State) ->
    start_ping_timer(stop_ping_timer(State)).

check_time_difference(State) ->
    PingTimeout = config:ping_timeout(config(State)),
    Delta = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(State#state.pong_timestamp),
    if
        Delta >= PingTimeout ->
            {ping_timeout, Delta};
        true ->
            ok
    end.
