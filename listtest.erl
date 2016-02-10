-module(listtest).
-compile(export_all).

test() ->
    L = [1,with,rec,8,9],
    exps(L).


exps([E0 | Es]) ->
    case (E0 =:= with) of
	false ->  E1 = exp(E0), [E1 | exps(Es)];
	true ->
	    [H| T] = Es,
   	    io:format("Rest ~p~n",[T]),
	    E1 = exp(E0),
	    [E1 | exps(T)]
    end;

exps([]) -> [].

exp(Pattern) ->
    Pattern.
