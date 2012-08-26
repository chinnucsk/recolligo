-module(config).
-export([new/3, new/4, new/5]).
-export([network/1, hostname/1, port/1, transport/1, nickname/1,
        alternative_nickname/1, username/1, realname/1, operator/1,
        operator_username/1, operator_password/1, ping_interval/1,
        ping_timeout/1, ircd_handler/1]).

-include("config.hrl").

new(Network, Hostname, Port) ->
    new(Network, Hostname, Port, ssl).

new(Network, Hostname, Port, Transport) ->
    new(Network, Hostname, Port, Transport, []).

new(Network, Hostname, Port, Transport, Options) ->
    #config { network = Network, hostname = Hostname, port = Port, transport = Transport, options = Options }.

network(#config { network = Network }) ->
    Network.

hostname(#config { hostname = Hostname }) ->
    Hostname.

port(#config { port = Port }) ->
    Port.

transport(#config { transport = Transport }) ->
    Transport.

nickname(#config { options = Options }) ->
    proplists:get_value(nickname, Options, "recolligo").

alternative_nickname(#config { options = Options }) ->
    case proplists:get_value(alternative_nickname, Options, "recolligo_") of
        Nickname when is_list(Nickname) ->
            Nickname;
        Fun when is_function(Fun) ->
            Fun();
        X ->
            X
    end.

username(#config { options = Options }) ->
    proplists:get_value(username, Options, "recolligo").

realname(#config { options = Options }) ->
    proplists:get_value(realname, Options, "Recolligo Bot").

operator(#config { options = Options }) ->
    proplists:get_bool(operator, Options).

operator_username(#config { options = Options }) ->
    proplists:get_value(operator_username, Options, "").

operator_password(#config { options = Options }) ->
    proplists:get_value(operator_password, Options, "").

ping_interval(#config { options = Options }) ->
    proplists:get_value(ping_interval, Options, 60).

ping_timeout(#config { options = Options }) ->
    proplists:get_value(ping_timeout, Options, 180).

ircd_handler(#config { options = Options }) ->
    proplists:get_value(ircd_handler, Options, ratbox_handler).
