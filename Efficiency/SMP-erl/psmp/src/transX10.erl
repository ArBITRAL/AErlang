-module(transX10).
-compile(export_all).
-define(WFile,"women_att.list").
-define(MFile,"men_att.list").
-define(FileOut,"SMTIX10.list").

print(Args) ->
    io:format("~p~n",[Args]).

mread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Prefs = proplists:get_value(prefs,Terms),
    Atts = proplists:get_value(atts,Terms),
    Index = lists:seq(1,length(Atts)),
    L1 = lists:map(fun(X) -> [{id,list_to_atom("m"++integer_to_list(X))}] end, Index),
    L3 = [build_plist(X) || X <- Prefs],
    MEnv = lists:zipwith3(fun(X,Y,Z) -> X ++ Y ++ [{prefs,Z}] end, L1, Atts, L3),
    MEnv.

wread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Prefs = proplists:get_value(prefs,Terms),
    Atts = proplists:get_value(atts,Terms),
    Index = lists:seq(1,length(Atts)),
    L1 = lists:map(fun(X) -> [{id,list_to_atom("w"++integer_to_list(X))}] end, Index),
    %%L3 = [build_wprefs(X) || X <- Prefs],
    WEnv = lists:zipwith3(fun(X,Y,Z) -> X ++ Y ++ [{prefs,Z}] end, L1, Atts, Prefs),
    WEnv.

start(Path) ->
    {ok, Women} = file:consult(Path++"/"++?WFile),
    {ok, Men} = file:consult(Path++"/"++?MFile),
    WPrefs = proplists:get_value(prefs,Women),
    WAtts = proplists:get_value(atts,Women),
    MPrefs = proplists:get_value(prefs,Men),
    MAtts = proplists:get_value(atts,Men),
    MIndex = lists:seq(1,length(MAtts)),
    WIndex = lists:seq(1,length(WAtts)),
    %% MIndex = lists:map(fun(X) -> [{id,list_to_atom("m"++integer_to_list(X))}] end, lists:seq(1,length(MAtts))),
    %% WIndex = lists:map(fun(X) -> [{id,list_to_atom("w"++integer_to_list(X))}] end, lists:seq(1,length(WAtts))),

    NewList = lists:zip(WIndex,WAtts),
    NewList1 = lists:zip(MIndex,MAtts),

    MList = build_men(MPrefs,NewList),
    [file_write(Path++"/"++?FileOut, X) || X <- MList],
    WList = build_men(WPrefs,NewList1),
    [file_write(Path++"/"++?FileOut, X) || X <- WList].

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

build_men(Prefs,List) ->
    build_men(Prefs,List,[]).

build_men([],_,Acc) ->
    lists:reverse(Acc);
build_men([H|T],WAtts,Acc) ->
    %% for each man with prefs H
    R = build_man(H,WAtts),
    build_men(T,WAtts,[R|Acc]).


%% For each man's preference, create a list of women accordingly
build_man(Prefs,WAtts) ->
    build_man(Prefs,WAtts,[],length(Prefs)).

build_man(_,WAtts,Acc,0) ->
    Acc;
build_man(Prefs,WAtts,Acc,N) ->
    H = limit_set(Prefs, N),
    R = lists:map(fun(SubH) ->
			  build_sub(SubH,WAtts)
		  end, H),
    NewAcc = lists:foldl(fun(L,Sum) ->
				 Sum ++ [L]
			 end, Acc, R),
    build_man(Prefs,WAtts, NewAcc ,N-1).


%% For each strict preference of the man, create a ties of  women accordingly
build_sub(SubH,WAtts) ->
    build_sub(SubH,WAtts,[]).

build_sub(_,[],Acc) ->
    [H|T] = lists:reverse(Acc),
    Acc1 = [0-X || X <- T],
    [H|Acc1];
build_sub(L,[{I,H}|T],Acc) ->
    R = lists:foldr(fun({A,V},Sum) ->
			Match = proplists:get_value(A,H),
			if Match == V ->
				Sum + 1;
			   true ->
				Sum
			end end, 0, L),
    Acc1 = if R == length(L) ->
	    [I|Acc];
	      true -> Acc
	   end,
    build_sub(L,T,Acc1).


build_women(Prefs,List) ->
    build_women(Prefs,List,[]).

build_women([],_,Acc) ->
    lists:reverse(Acc);
build_women([H|T],MAtts,Acc) ->
    %% for each woman with prefs H
    R = build_woman(H,MAtts),
    build_women(T,MAtts,[R|Acc]).


build_woman(H,MAtts) ->
    build_woman(H,MAtts,[]).


build_woman(_,[],Acc) ->
    New = lists:reverse( lists:sort(proplists:get_keys(Acc))),
    Acc1 = lists:map(fun(Key) ->
		      proplists:append_values(Key,Acc)
	      end, New),
    Acc1;
build_woman(L,[{I,H}|T],Acc) ->
    R = lists:foldr(fun({A,V}, Sum) ->
			    Match = proplists:get_value(A,H),
			    if Match == V ->
				    Sum + 1;
			       true ->
				    Sum
			    end
		    end, 0, L),
    build_woman(L,T, [{R,I}|Acc]).



score(L, W, B) ->
    X = proplists:get_value(wealth,L),
    Y = proplists:get_value(body,L),
    Score = case {X,Y} of
		{W,B} -> 3;
		{W,_} -> 2;
		{_,B} -> 1;
		_Other ->0
	    end,
    Score.


build_plist(Prefs) when is_list(Prefs) ->
    build_plist(Prefs,[],length(Prefs)).

build_plist(_, R, 0) ->
    R;
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


build_wprefs(Prefs) ->
    L = lists:map(fun({A,V}) ->
			   V1 = if is_atom(V) -> "_" ++ atom_to_list(V);
				   is_integer(V) -> integer_to_list(V);
				   true -> V
				end,
			   atom_to_list(A) ++ "=" ++ V1 end, Prefs),
    string:join(L," and ").

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

file_write(File, []) ->
    ok;
file_write(File, List) ->
    file:write_file(File, lists:foldl(fun(E, A) ->
        A ++ "\n" ++ E
    end, hd(List), tl(List)),[append]).
