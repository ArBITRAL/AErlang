-module(gen_input).
-compile(export_all).
-define(CONF,"conf").

print(X) ->
    io:format("~p~n",[X]).

start() ->
    {ok, [M1,M2]} = file:consult(?CONF),
    %%print(M1),
    %%print(M2),
    {Name1, Num1, [{atts, Atts1}, {domain, Domain1}], [{prefs,Prefs1},{domain,DomainP}]} = M1,
    {Name2, Num2, [{atts, Atts2}, {domain, Domain2}], [{prefs,Prefs2},{domain,DomainQ}]} = M2,

    test_input(Domain1),
    test_input(Domain2),
    test_input(DomainP),
    test_input(DomainQ),

    DA1 = [rounding(A,Num1) || A <- Domain1],
    DP1 = [rounding(A,Num1) || A <- DomainP],

    DA2 = [rounding(A,Num2) || A <- Domain2],
    DP2 = [rounding(A,Num2) || A <- DomainQ],

    Random = erlang:unique_integer(),
    random:seed(Random),

    gen(Name1,Num1,lists:zip(Atts1,DA1),lists:zip(Prefs1,DP1)),
    gen(Name2,Num2,lists:zip(Atts2,DA2),lists:zip(Prefs2,DP2)).


test_input(List) ->
    Test = lists:map(fun(L) ->
			     Sum = lists:sum([P || {_,P} <- L]),
			     [New] = io_lib:format("~.10f",[Sum]),
			     1.0 >= list_to_float(New)
		  end,List),
    case lists:member(false,Test) of
	true -> error("Wrong probabilities value");
	false -> ok
    end.

gen(Name,Num,Merged,Merged1) ->
    %% attribute generate
    L1 =
	lists:foldl(fun({A,P},Acc) ->
			  {Choosen,_} = choose(P,Num),
			  Acc ++ [{I,{A,V}} || {I,V} <- Choosen]
		  end, [], Merged),
    Atts = [proplists:append_values(I,L1) || I <- lists:seq(1,Num)],
    %%preference generate
    L2 =
	lists:foldl(fun({A,P},Acc) ->
			  {Choosen,_} = choose(P,Num),
			  Acc ++ [{I,{A,V}} || {I,V} <- Choosen]
		  end, [], Merged1),
    Prefs = [proplists:append_values(I,L2) || I <- lists:seq(1,Num)],
    FileName = atom_to_list(Name) ++ "_att.list",
    Print=[{atts,Atts},{prefs,Prefs}],
    write_terms(FileName,Print).

choose(List,Num) ->
    %%print(List),
    Sample = lists:foldl(fun({A,V},Sum) ->
				 lists:duplicate(V,A) ++ Sum end, [], List),
    %%print(Sample),
    lists:mapfoldl(fun(X,Acc) ->
			Coin = random:uniform(length(Acc)),
			H = lists:nth(Coin,Acc),
			{{X,H},lists:delete(H,Acc)}
		  end, Sample, lists:seq(1,length(Sample))).

err(Actual, Rounded) ->
    if Actual == 0 ->
	    0;
       true ->
	    Divisor = math:sqrt(if Actual < 1.0 ->
					1.0;
				   true -> Actual end),
	    Sqr = fun(X) -> X*X end,
	    Sqr(abs(Rounded - Actual)) / Divisor
    end.


rounding(List,Num) ->
    Len = lists:seq(1,length(List)),
    Rounded = [{V,trunc(P*Num)} || {V,P} <- List],
    Actual = [{V,P*Num} || {V,P} <- List],
    %%print(Actual),
    %% Solving sum of pi < 1.0
    Num1 = round(Num*(lists:sum([P || {_,P} <- List]))),
    %%print(Num1),
    Up = Num1 - lists:sum([P || {_,P} <- Rounded]),
    Err = lists:zipwith3(fun(I,{_,X},{_,Y}) ->
				 {I,err(X,Y + 1) - err(X,Y)} end,
			 Len,Actual,Rounded),
    Rank = lists:keysort(2,Err),
    %%print(lists:zip(Len,Rounded)),
    increase(Rank,lists:zip(Len,Rounded),Up).

increase(_,List,0) ->
    [Pair || {_,Pair} <- List];
increase([{H,_}|T],List,N) ->
    {V,Val} = proplists:get_value(H,List),
    NewList = lists:keyreplace(H,1,List,{H,{V,Val+1}}),
    increase(T,NewList,N-1).

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).
