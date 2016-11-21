-module(psmp).
-compile(export_all).


run(_,_,[]) -> init:stop();
run(N,PATH,[H|T]) ->
    Start1 = os:timestamp(),
    Path = PATH++H,
    io:format("Solving problem in ~p ~n",[Path]),
    WomenList = read:wreadlines(Path++"/women.list"),
    MenList = read:mreadlines(Path++"/men.list"),
    io:format("IO time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start1)/1000]),
    ok = execute(N,WomenList,MenList),
    run(N,PATH,T).

execute(0,_,_) -> ok;
execute(N,WomenList,MenList) ->
    io:format("~n ======= Run ~p: =========~n",[N]),
    Start2 = os:timestamp(),
    [Wpids,Mpids] = init(WomenList,MenList),
    io:format("Init time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start2)/1000]),
    Start3 = os:timestamp(),
    ok = start(WomenList,MenList,Wpids,Mpids),
    io:format("Computation time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start3)/1000 - 1000]),
    execute(N-1,WomenList,MenList).


init(WomenList,MenList) ->
    Wpids = lists:foldr(fun({Id,Prefs},R) ->
    			  [pwoman:init(Id, Prefs) | R] end, [],
    			  WomenList),
    io:format("Woman ~p~n",[length(Wpids)]),
    Mpids = lists:foldr(fun({Id,Prefs},R) ->
				[pman:init(Id,Prefs) | R] end, [],
			MenList),
    io:format("Man ~p~n",[length(Mpids)]),
    Wid = [Id || {Id,_} <- WomenList],
    %io:format("Wid ~p~n",[Wid]),
    L1 = lists:zip(Wid,Wpids),
    %io:format("L1 ~p~n",[L1]),
    lists:foreach(fun({Id,P}) ->
    			  global:register_name(Id,P) end, L1),
    Mid = [Id || {Id,_} <- MenList],
    %io:format("Mid ~p~n",[Mid]),
    L2 = lists:zip(Mid,Mpids),
    %io:format("L2 ~p~n",[L2]),
    lists:foreach(fun({Id,P}) ->
    			  global:register_name(Id,P) end, L2),

    global:register_name(log,self()),
    [Wpids,Mpids].

start(WomenList,MenList,Wpids,Mpids) ->
    lists:foreach(fun(X) -> X ! start end, Wpids),
    lists:foreach(fun(X) -> X ! start end, Mpids),
    log(maps:new(),WomenList,MenList,Wpids,Mpids),
    ok.

log(State,WomenList,MenList,Wpids,Mpids) ->
    receive
	{M,W} ->
	    log(maps:put(W,{M,W},State),WomenList,MenList,Wpids,Mpids)
    after
	1000 ->
	    lists:foreach(fun(X) -> exit(X,kill) end, Mpids),
	    lists:foreach(fun(X) -> exit(X,kill) end, Wpids),
	    %List = maps:values(State),
	    io:format("Matching ~n. Size ~p~n",[maps:size(State)]),
	    io:format("Stable ~p ~n",[stable:check(maps:values(State),WomenList,MenList)])
	    %% Seq = lists:seq(1,length(WomenList)),
	    %% Full = lists:map(fun(X) ->
	    %% 			      list_to_atom("m"++ integer_to_list(X)) end, Seq),
	    %% Part = lists:foldl(fun({M,_},Sum) -> [M] ++ Sum end, [], List),
	    %% io:format("Single men ~p ~n", [Full -- Part])
    end.
