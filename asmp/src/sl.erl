-module(sl).
-compile(export_all).

sl(Path,Size) ->
    %{ok, Type} = application:get_env(asmp,type),
    PATH = Path ++ "size-" ++ integer_to_list(Size),
   % io:format("PATH is ~p ~n",[PATH]),
    {ok, DirList} = file:list_dir(PATH),
   % io:format("Dirlist is ~p ~n",[DirList]),
    getsl(PATH,DirList).


getsl(_,[]) -> ok;
getsl(PATH,[H|T]) ->
    Path = PATH++"/"++H,
    {ok, DirList} = file:list_dir(Path),
    %io:format("Dirlist is ~p ~n",[DirList]),
    getsl1(Path,DirList),
    getsl(PATH,T).

getsl1(PATH,[]) -> ok;
getsl1(PATH,[H|T]) ->
    Path = PATH++"/"++H,
    io:format("\n===============================================\n"),
    io:format("Compute Lv. of sat. in ~p",[Path]),
    io:format("\n===============================================\n"),
    Wc = read1:wread(Path++"/women_prefs.list"),
    Mc = read1:mread(Path++"/men_prefs.list"),
%    io:format("Mlist is ~p ~n",[Mc]),
    {ok,M} = file:consult(Path++"/matching.txt"),
    L=compute(M,Wc,Mc),
    TM = lists:sum([Ms || {Ms,_} <- L]),
    TW = lists:sum([Ws || {_,Ws} <- L]),
    TP = TM + TW,
    io:format("Man Level ~p~nWoman Level ~p~nTotal ~p~n",[TM,TW,TP]),
    getsl1(PATH,T).


compute([H|T],Wc,Mc) ->
    compute([H|T],Wc,Mc,[]).


compute([],Wc,Mc,Acc) ->
    Acc;
compute([H|T],Wc,Mc,Acc) ->
    {Ms,Ws}=mapl(H,Wc,Mc),
    compute(T,Wc,Mc,[{Ms,Ws}|Acc]).

mapl({M,W},Wc,Mc) ->
    WL = lists:reverse(proplists:get_value(M,Mc)),
    Ms = getscore(W,WL),
    ML = lists:reverse(proplists:get_value(W,Wc)),
    Ws = getscore(M,WL),
    {Ms,Ws}.


getscore(W,[]) -> 0;
getscore(W,[H|T]) ->
    case lists:member(W,H) of
	true -> 2;
	false ->
	    2 + getscore(W,T)
    end.
