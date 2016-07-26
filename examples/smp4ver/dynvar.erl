-module(dynvar).
-export([with/2, fetch/1, test/0]).

with(Bindings, Action) ->
    lists:foreach(fun({Name, Value}) -> put(Name, Value) end, Bindings),
    try apply(Action, []) of
        X -> X
    after
        lists:foreach(fun({Name, _Value}) -> erase(Name) end, Bindings)
    end.

fetch(VarName) ->
    get(VarName).

player_greeting() ->
    dynvar:fetch(player).

test() ->
    Greeting = dynvar:with([{player, "Dr. Falken"}],
                           fun player_greeting/0),
    Greeting.
