-module(read_att).
-compile([export_all]).

print(Args) ->
    io:format("~p~n",[Args]).

mread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Prefs = proplists:get_value(prefs,Terms),
    Atts = proplists:get_value(atts,Terms),
    Index = lists:seq(1,length(Atts)),
    L1 = lists:map(fun(X) -> [{id,list_to_atom("m"++integer_to_list(X))}] end, Index),
    L3 = [build_plist(X) || X <- Prefs],
    MEnv = lists:zipwith3(fun(X,Y,Z) -> [X ++ Y] ++ [{prefs,Z}] end, L1, Atts, L3),
    MEnv.

wread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Prefs = proplists:get_value(prefs,Terms),
    Atts = proplists:get_value(atts,Terms),
    Index = lists:seq(1,length(Atts)),
    L1 = lists:map(fun(X) -> [{id,list_to_atom("w"++integer_to_list(X))}] end, Index),
    %%L3 = [build_plist(X) || X <- Prefs],
    WEnv = lists:zipwith3(fun(X,Y,Z) -> [X ++ Y] ++ [{prefs,Z}] end, L1, Atts, Prefs),
    WEnv.

build_plist(Prefs) when is_list(Prefs) ->
    build_plist(Prefs,[],length(Prefs)).

build_plist(_, [H,T|R], 0) ->
    [H,T|R];
build_plist(Prefs, Result, N) ->
    H = limit_set(Prefs, N),
    L = lists:foldr(fun(L1,Acc) ->
			    L2 = lists:map(fun({A,V}) ->
				 V1 = if is_atom(V) -> "_" ++ atom_to_list(V);
					  is_integer(V) -> integer_to_list(V);
					  true -> V
				      end,
					      atom_to_list(A) ++ "=" ++ V1 end, L1),
			    Pred = string:join(L2," and "),
			    [Pred|Acc]
		    end, [], H),
    build_plist(Prefs,Result++L,N-1).

generate([]) -> [[]];
generate([H|T]) ->
    PT = generate(T),
    generate(H, PT, PT).

generate(_, [], Acc) -> Acc;
generate(X, [H|T], Acc) -> generate(X, T, [[X|H]|Acc]).


limit_set(L, N) ->
  {_, Acc} = generate1(L, N),
  Acc.

generate1([], _) ->
    {[[]], []};
generate1([H|T], N) ->
    {PT, Acc} = generate1(T, N),
    generate1(H, lists:reverse(PT), PT, Acc, N).

generate1(_, [], PT, Acc, _) ->
  {PT, Acc};
generate1(X, [H|T], PT, Acc, N) when length(H)=/=N-1 ->
  generate1(X, T, [[X|H]|PT], Acc, N);
generate1(X, [H|T], PT, Acc, N) ->
  generate1(X, T, [[X|H]|PT], [[X|H]|Acc], N).
