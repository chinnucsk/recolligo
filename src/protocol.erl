-module(protocol).
-export([render/1, parse/1]).

-include("protocol.hrl").

render(#message{ prefix = Prefix, command = Command, arguments = Argument}) ->
    render(Prefix, string:to_upper(atom_to_list(Command)), lists:map(fun binary_to_list/1, Argument)).

render(<<>>, Command, Arguments) ->
    io_lib:format("~s ~s", [Command, string:join(Arguments, " ")]);
render(Prefix, Command, Arguments) ->
    io_lib:format(":~s ~s ~s", [Prefix, Command, string:join(Arguments, " ")]).

parse(X) when is_binary(X) ->
    parse_prefix(X);
parse(X) when is_list(X) ->
    parse(list_to_binary(X)).

parse_prefix(<<>>) ->
    {error, {empty_line}};
parse_prefix(<<$:, X/binary>>) ->
    case binary:split(X, <<" ">>) of
        [Prefix, Rest] ->
            parse_command(Prefix, Rest);
        _Otherwise ->
            {error, {missing_command, X}}
    end;
parse_prefix(X) ->
    parse_command(<<>>, X).

parse_command(Prefix, X) ->
    case binary:split(X, <<" ">>) of
        [Command, Rest] ->
            {ok, #message { prefix = Prefix, command = command_to_atom(Command), arguments = parse_arguments(Rest) }};
        [Command] ->
            {ok, #message { prefix = Prefix, command = command_to_atom(Command), arguments = [] }};
        _Otherwise ->
            {error, {missing_command, X}}
    end.

parse_arguments(<<$:, X/binary>>) ->
    [X];
parse_arguments(<<X/binary>>) ->
    case binary:split(X, <<" ">>) of
        [Argument, Rest] ->
            [Argument | parse_arguments(Rest)];
        _Otherwise ->
            [X]
    end.

command_to_atom(X) when is_binary(X) ->
    command_to_atom(binary_to_list(X));
command_to_atom(X) when is_list(X) ->
    list_to_atom(string:to_lower(X)).
