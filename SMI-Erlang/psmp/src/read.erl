-module(read).
-compile(export_all).

wreadlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    List = binary:split(Data, [<<"\n">>], [global]),
    L = lists:map(fun(X) ->
			  Y = binary_to_list(X),
			  re:split(Y,"[\s]+",[{return,list}])
		  end, List),
    L1 = lists:filter(fun(X) ->
			      case X of
				  [[]] -> false;
				  _ -> true
			      end
		       end, L),

    L2 = lists:foldr(fun(X,Sum) ->
			     [lists:map(fun(Y) -> list_to_integer(Y) end, X)] ++ Sum end,
		     [], L1),
    %io:format("list ~p~n",[L2]),
    Prefs = lists:map(fun(X) -> wsplit(X,[]) end, L2),
    Seq = lists:seq(1,length(Prefs)),
    Index = lists:map(fun(X) ->
			  list_to_atom("w"++ integer_to_list(X)) end, Seq),
    lists:zip(Index,Prefs).


wsplit([],Global) -> Global;
wsplit([H|T],Global) ->
    case H > 0 of
	true ->
	    {New,Result} = whandle(T,list_to_atom("m"++ integer_to_list(H))),
	    wsplit(New,Global ++ [Result]);
	false ->
	    Global
    end.

whandle([], Result) -> {[],Result};
whandle([H|T], Result) when H > 0 -> {[H|T], Result};
whandle([H|T], Result) when H < 0 ->
    case is_list(Result) of
	true -> whandle(T, Result ++ [list_to_atom("m"++ integer_to_list(-H))]);
	false -> whandle(T, [Result,list_to_atom("m"++ integer_to_list(-H))])
    end.


mreadlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    List = binary:split(Data, [<<"\n">>], [global]),
    L = lists:map(fun(X) ->
			  Y = binary_to_list(X),
			  re:split(Y,"[\s]+",[{return,list}])
		  end, List),
    L1 = lists:filter(fun(X) ->
			      case X of
				  [[]] -> false;
				  _ -> true
			      end
		       end, L),

    L2 = lists:foldr(fun(X,Sum) ->
			     [lists:map(fun(Y) -> list_to_integer(Y) end, X)] ++ Sum end,
		     [], L1),
    %io:format("list ~p~n",[L2]),
    Prefs = lists:map(fun(X) -> msplit(X,[]) end, L2),
    Seq = lists:seq(1,length(Prefs)),
    Index = lists:map(fun(X) ->
			  list_to_atom("m"++ integer_to_list(X)) end, Seq),
    lists:zip(Index,Prefs).


msplit([],Global) -> Global;
msplit([H|T],Global) ->
    case H > 0 of
	true ->
	    {New,Result} = mhandle(T,list_to_atom("w"++ integer_to_list(H))),
	    msplit(New,Global ++ [Result]);
	false ->
	    Global
    end.

mhandle([], Result) -> {[],Result};
mhandle([H|T], Result) when H > 0 -> {[H|T], Result};
mhandle([H|T], Result) when H < 0 ->
    case is_list(Result) of
	true -> mhandle(T, Result ++ [list_to_atom("w"++ integer_to_list(-H))]);
	false -> mhandle(T, [Result,list_to_atom("w"++ integer_to_list(-H))])
    end.
