-module(irc).
-export([render/1]).

render({nick, Nickname}) ->
    io_lib:format("NICK ~s", [Nickname]);
render({quit, Message}) ->
    io_lib:format("QUIT ~s", [Message]);
render({pong, Message}) ->
    io_lib:format("PONG ~s", [Message]);
render({ping, Hostname}) ->
    io_lib:format("PING ~s", [Hostname]);
render({oper, Username, Password}) ->
    io_lib:format("OPER ~s ~s", [Username, Password]);
render({whois, Nickname}) ->
    io_lib:format("WHOIS ~s", [Nickname]);
render({whois, A, B}) ->
    io_lib:format("WHOIS ~s ~s", [A, B]);
render({notice, Destination, Message}) ->
    io_lib:format("NOTICE ~s :~s", [Destination, Message]);
render({mode, Target, Mode}) ->
    io_lib:format("MODE ~s ~s", [Target, Mode]);
render({user, Username, Hostname, Servername, Realname}) ->
    io_lib:format("USER ~s ~s ~s :~s", [Username, Hostname, Servername, Realname]).
