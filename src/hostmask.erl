-module(hostmask).
-export([stringify/3, parse/1, nickname/1, username/1, hostname/1]).

stringify(Nickname, Username, Hostname) ->
    io_lib:format("~s!~s@~s", [Nickname, Username, Hostname]).

parse_nickname_hostname([NicknameAndUsername, Hostname]) ->
    case string:tokens(NicknameAndUsername, "!") of
        [Nickname, Username] ->
            {Nickname, Username, Hostname};
        _Otherwise ->
            {error, invalid_hostmask}
    end.

parse(X) when is_list(X)->
    parse_nickname_hostname(string:tokens(X, "@"));
parse(X) when is_binary(X) ->
    parse(binary_to_list(X)).

nickname(X) ->
    case parse(X) of
        {Nickname, _, _} ->
            Nickname;
        Error ->
            Error
    end.

username(X) ->
    case parse(X) of
        {_, Username, _} ->
            Username;
        Error ->
            Error
    end.

hostname(X) ->
    case parse(X) of
        {_, _, Hostname} ->
            Hostname;
        Error ->
            Error
    end.
