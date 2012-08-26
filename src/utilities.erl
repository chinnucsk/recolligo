-module(utilities).
-export([random_nickname/1, relative_time_difference/2]).

random_nickname(0) ->
    [];
random_nickname(N) ->
    [random_character() | random_nickname(N - 1)].

random_character() ->
    crypto:rand_uniform(97, 123).

relative_time_difference(T1, T2) ->
    Fudge = 1.30,
    Delta = calendar:datetime_to_gregorian_seconds(T2) - calendar:datetime_to_gregorian_seconds(T1),
    if
        Delta < (1 * Fudge) ->
            io_lib:format("about a second ago", []);
        Delta < (60 * (1 / Fudge)) ->
            io_lib:format("about ~B seconds ago", [round(Delta)]);
        Delta < (60 * Fudge) ->
            io_lib:format("about a minute ago", []);
        Delta < (60 * 60 * (1 / Fudge)) ->
            io_lib:format("about ~B minutes ago", [round(Delta / 60)]);
        Delta < (60 * 60 * Fudge) orelse 1 == Delta / (60 * 60) ->
            io_lib:format("about an hour ago", []);
        Delta < (60 * 60 * 24 * (1 / Fudge)) ->
            io_lib:format("about ~B hours ago", [round(Delta / (60 * 60))]);
        Delta < (60 * 60 * 24 * Fudge) orelse 1 == Delta / (60 * 60 * 24) ->
            io_lib:format("about a day ago", []);
        true ->
            io_lib:format("about ~B days ago", [round(Delta / (60 * 60 * 24))])
    end.
