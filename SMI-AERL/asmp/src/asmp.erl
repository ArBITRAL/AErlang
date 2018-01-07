-module(asmp).
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
    ets:new(?MODULE,[named_table,public,{write_concurrency,true}, {read_concurrency,false},{keypos,2}]),
    Start2 = os:timestamp(),
    ok = aerl:start(),
    [Wpids,Mpids] = init(WomenList,MenList),
    io:format("Init time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start2)/1000]),
    Start3 = os:timestamp(),
    %% Start solving problem
    ok = start(WomenList,MenList,Wpids,Mpids),
    io:format("Computation time=~p seconds~n",[timer:now_diff(os:timestamp(), Start3)/1000000]),
    Start4 = os:timestamp(),
    State = ets:tab2list(?MODULE),
    io:format("Stable ~p ~n",[stable:check(State,WomenList,MenList)]),
    io:format("Stable check time=~p seconds~n",[timer:now_diff(os:timestamp(), Start4)/1000000]),
    %% Clean up
    [exit(X,kill) || X <- Mpids],
    [exit(X,kill) || X <- Wpids],
    true = ets:delete(?MODULE),
    ok = aerl:stop(),
    execute(N-1,WomenList,MenList).

init(WomenList,MenList) ->
    Wpids = lists:foldr(fun({Id,Prefs},R) ->
    			  [awoman:init(#{id => Id, partner => none, sex => woman},Prefs) | R] end, [],
    			  WomenList),
    io:format("Woman ~p~n",[length(Wpids)]),
    Mpids = lists:foldr(fun({Id,Prefs},R) ->
    			  [aman:init(#{id => Id, partner => none, sex => man},Prefs) | R] end, [],
    			  MenList),
    io:format("Man ~p~n",[length(Mpids)]),
    Wid = [Id || {Id,_} <- WomenList],
    %io:format("Wid ~p~n",[Wid]),
    L1 = lists:zip(Wid,Wpids),
    %io:format("L1 ~p~n",[L1]),
    lists:foreach(fun({Id,P}) ->
			  ok = aerl:register(P,#{id => Id, partner => none, sex => woman}),true = register(Id,P) end, L1),
    Mid = [Id || {Id,_} <- MenList],
    %io:format("Mid ~p~n",[Mid]),
    L2 = lists:zip(Mid,Mpids),
    %io:format("L2 ~p~n",[L2]),
    lists:foreach(fun({Id,P}) ->
    			  ok = aerl:register(P,#{id => Id, partner => none, sex => man}),true = register(Id,P) end, L2),
    global:register_name(log,self()),
    [Wpids,Mpids].

start(WomenList,MenList,Wpids,Mpids) ->
    %% Start processes
    [X ! start || X <- Wpids],
    [X ! start || X <- Mpids],
    log(WomenList,MenList),
    ok.


log(WomenList,MenList) ->
    timer:sleep(10),
    Size = proplists:get_value(size,ets:info(?MODULE)),
    %% io:format("Size ~p ~n",[Size]),
    case Size == length(WomenList) of
	true ->
	    State = ets:tab2list(?MODULE),
	    output(State,WomenList,MenList);
	false ->
	    log(WomenList,MenList)
    end.

output(State,WomenList,MenList) ->
    io:format("Matching Size ~p~n",[length(State)]).
