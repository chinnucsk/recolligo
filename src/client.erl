-module(client).
-export([start/1, start/3, stop/1]).
-export([quit/2, ping/1, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-define(PING_INTERVAL, 60).
-define(PING_TIMEOUT, 180).

-include("protocol.hrl").
-include("client.hrl").

start(Network, Hostname, Port) ->
    start(#configuration{ network = Network, hostname = Hostname, port = Port }).

start(#configuration{ network = Network } = Configuration) ->
    gen_server:start_link({local, Network}, ?MODULE, Configuration, []).

stop(Network) when is_atom(Network) ->
    gen_server:call(Network, stop).

quit(Network, Message) ->
    gen_server:call(Network, {quit, Message}).

ping(Network) ->
    gen_server:cast(Network, ping).

init(Config) ->
    case ssl:connect(Config#configuration.hostname, Config#configuration.port, [binary, {packet, 0}, {active, once}]) of
        {ok, Socket} ->
            irc:send(Socket, Config#configuration.network, {nick, "ahfbot"}),
            irc:send(Socket, Config#configuration.network, {user, "insight", "XXX", "XXX", "IRC Monitor"}),
            State = #state{ continuation = <<>>,
                            configuration = Config,
                            socket = Socket,
                            ping_timer = undefined,
                            pong_timestamp = erlang:localtime(),
                            ircd_handler = ratbox_handler },
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
            C = State#state.configuration,
            send(State, {ping, C#configuration.hostname}),
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
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ssl:close(State#state.socket),
    stop_ping_timer(State),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

ack_socket(Socket) ->
    ssl:setopts(Socket, [{active, once}]).

handle_packet(Chunk, #state { continuation = Cont, socket = Socket  } = State) ->
    ack_socket(Socket),
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
    IrcdHandler = State#state.ircd_handler,
    case protocol:parse(Line) of
        {ok, Message} ->
            NewState2 = handle_message(NewState, Message),
            IrcdHandler:handle_message(NewState2, Message);
        {error, _} ->
            NewState
    end.

handle_message(State, Message) ->
    C = State#state.configuration,
    io:format("(~s) -> ~s~n", [atom_to_list(C#configuration.network), protocol:render(Message)]),
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
    C = State#state.configuration,
    irc:send(State#state.socket, C#configuration.network, Message),
    State.

start_ping_timer(State) ->
    C = State#state.configuration,
    {ok, PingTimer} = timer:apply_after(?PING_INTERVAL * 1000, client, ping, [C#configuration.network]),
    State#state{ ping_timer = PingTimer }.

stop_ping_timer(#state { ping_timer = PingTimer } = State) ->
    timer:cancel(PingTimer),
    State#state{ ping_timer = undefined }.

reset_ping_timer(State) ->
    start_ping_timer(stop_ping_timer(State)).

check_time_difference(State) ->
    Delta = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(State#state.pong_timestamp),
    if
        Delta >= ?PING_TIMEOUT ->
            {ping_timeout, Delta};
        true ->
            ok
    end.
