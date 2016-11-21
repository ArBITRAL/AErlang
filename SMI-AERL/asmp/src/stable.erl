-module(stable).
-export([check/3]).

check(Matching,WomenList,MenList) ->
    man_check(Matching,WomenList,MenList,Matching).

man_check([],_,_,_) -> true;
man_check([H|T],WL,ML,Matching) ->
    {M,W} = H,
    %io:format("Check the pair {~p,~p}~n",[M,W]),
    List = get(M,ML),
    MList = list(W,List,[]),
    %io:format("Women who ~p prefer more ~p~n",[M,MList]),
    case woman_check(MList,M,Matching,WL) of
	pass -> man_check(T,WL,ML,Matching);
	Woman -> io:format("Blocking pair (~p,~p)~n",[M,Woman]),false
    end.

woman_check([],_,_,_) -> pass;
woman_check([H|_T],M,Matching,WL) when is_list(H) ->
    woman_check(H,M,Matching,WL);
woman_check([H|T],M,Matching,WL) ->
    %io:format("Now woman ~p check~n",[H]),
    Partner = getwpartner(H,Matching),
    %io:format("Her partner is ~p~n",[Partner]),
    L = get(H,WL),
    case bof(L,Partner,M) of
	true ->
	    woman_check(T,M,Matching,WL);
	false -> H
    end.

bof([], _, _) -> true;
bof([H|T], Partner, Y) ->
  case H of
      Y -> false;
      Partner -> true;
      List when is_list(List) ->
	  case {lists:member(Y,List), lists:member(Partner,List)} of
	      {true, true} -> true;
	      {true, false} -> false;
	      {false, true} -> true;
	      _ ->
		  bof(T, Partner, Y)
	  end;
      _ -> bof(T, Partner, Y)
  end.

getwpartner(_,[]) -> [];
getwpartner(M,[H|T]) ->
    case H of
	{List, M} ->
	    List;
	_ -> getwpartner(M,T)
    end.


get(_,[]) -> [];
get(M,[H|T]) ->
    case H of
	{M, List} ->
	    List;
	_ -> get(M,T)
    end.

list(_,[],Result) -> Result;
list(W,[H|T],Result) when is_list(H) ->
     case lists:member(W,H) of
	 true -> Result;
	 false -> list(W,T,[H] ++ Result)
    end;
list(W,[H|T],Result) ->
    case H of
	W -> Result;
	_ -> list(W,T,[H] ++ Result)
    end.
