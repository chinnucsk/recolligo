-module(command_handler).
-export([handle/4]).

-include("client.hrl").

handle(Hostmask, uptime, _Arguments, State) ->
    Nickname = hostmask:nickname(Hostmask),
    Message = io_lib:format("I was started ~s.", [uptime:relative_uptime()]),
    client:send(State, {notice, Nickname, Message});
handle(Hostmask, echo, Arguments, State) ->
    Nickname = hostmask:nickname(Hostmask),
    Message = string:join(Arguments, " "),
    client:send(State, {notice, Nickname, Message});
handle(Hostmask, Command, _Arguments, State) ->
    Message = io_lib:format("Unknown Command: ~s ...", [atom_to_list(Command)]),
    Nickname = hostmask:nickname(Hostmask),
    client:send(State, {notice, Nickname, Message}).
