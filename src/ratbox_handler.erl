-module(ratbox_handler).
-export([handle_message/2]).
-behaviour(ircd_handler).

-include("protocol.hrl").

handle_message(State, Message) ->
    handle_one_message(State, Message#message.prefix, Message#message.command, Message#message.arguments).
handle_one_message(_State, _Prefix, '491', _Arguments) ->
    throw(operator_authentication_failed);
handle_one_message(State, _Prefix, notice, [<<"*">>, Arguments]) ->
    handle_server_notice(State, parse_server_notice(Arguments));
handle_one_message(State, _Prefix, _Command, _Arguments) ->
    State.

handle_server_notice(State, {client_connecting, Name, User, _, Ip, _, _}) ->
    Hostmask = hostmask:stringify(Name, User, Ip),
    store:connecting_client(efnet, Hostmask),
    State;
handle_server_notice(State, {client_disconnecting, Name, User, _, Ip, _}) ->
    Hostmask = hostmask:stringify(Name, User, Ip),
    store:disconnecting_client(efnet, Hostmask),
    State;
handle_server_notice(State, _Arguments) ->
    State.

task_to_atom(<<"CLIEXIT">>) -> client_disconnecting;
task_to_atom(<<"CLICONN">>) -> client_connecting;
task_to_atom(_) -> unknown.

parse_server_notice(<<"*** Notice -- ", Rest/binary>>) ->
    case binary:split(Rest, <<" ">>, []) of
        [Task, Rest2] ->
            parse_server_notice_task(task_to_atom(Task), Rest2);
        _Otherwise ->
            {error, {failed_parsing_task, Rest}}
    end.

parse_server_notice_task(client_connecting, Rest) ->
    case binary:split(Rest, <<" ">>, [global]) of
        [Name, User, Host, Ip, Class | Unusued] ->
            {client_connecting, binary_to_list(Name), binary_to_list(User), binary_to_list(Host), binary_to_list(Ip), binary_to_list(Class), Unusued};
        _Otherwise ->
            {error, {failed_parsing_client_connecting, Rest}}
    end;
parse_server_notice_task(client_disconnecting, Rest) ->
    case binary:split(Rest, <<" ">>, [global]) of
        [Name, User, Host, Ip, <<"0">> | Comment] ->
            {client_disconnecting, binary_to_list(Name), binary_to_list(User), binary_to_list(Host), binary_to_list(Ip), Comment};
        _Otherwise ->
            {error, {failed_parsing_client_disconnecting, Rest}}
    end;
parse_server_notice_task(unknown, _Rest) ->
    {unknown_server_notice}.
