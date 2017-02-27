-module(asmp).
-compile(export_all).

print(X) ->
    io:format("~p~n",[X]).

run(_,_,[]) -> init:stop();
run(N,PATH,[H|T]) ->
    Start1 = os:timestamp(),
    Path = PATH++H,
    io:format("Solving problem in ~p ~n",[Path]),
    transinput:start(Path),
    W = read1:wread(Path++"/women_prefs.list"),
    M = read1:mread(Path++"/men_prefs.list"),

    %% Data from X10
    %% W = read:wreadlines(Path++"/women.list"),
    %% M = read:mreadlines(Path++"/men.list"),

    %% print(W),
    %% print(M),
    %% W = read_att:wread(Path++"/women_att.list"),
    %% M = read_att:mread(Path++"/men_att.list"),
    %print(W),
    %print(M),
    %%print(WomenList),
    %%print(MenList),
    io:format("IO time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start1)/1000]),
    ok = execute(N,W,M),
    run(N,PATH,T).

execute(0,_,_) -> ok;
execute(N,WomenList,MenList) ->
    io:format("~n ======= Run ~p: =========~n",[N]),
    ok = aerl:start(),
    ets:new(?MODULE,[named_table,public,{write_concurrency,true}, {read_concurrency,false},{keypos,2}]),
    ets:new(message,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:insert(message,{num,0}),
    Start2 = os:timestamp(),
    [Wpids,Mpids] = init(WomenList,MenList),
    io:format("Init time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start2)/1000]),
    Start3 = os:timestamp(),
    %% Start solving problem
    Matching = start(WomenList,MenList,Wpids,Mpids),
    io:format("Computation time=~p seconds~n",[timer:now_diff(os:timestamp(), Start3)/1000000]),
    io:format("Number of Messages=~p~n",[ets:lookup_element(message,num,2)]),
    Start4 = os:timestamp(),
    output(Matching,WomenList,MenList),
    io:format("Stability check time=~p seconds~n",[timer:now_diff(os:timestamp(), Start4)/1000000]),
    %% Clean up
    true = ets:delete(?MODULE),
    true = ets:delete(message),
    [exit(X,kill) || X <- Mpids],
    [exit(X,kill) || X <- Wpids],
    ok = aerl:stop(),
    execute(N-1,WomenList,MenList).

init(WomenList,MenList) ->
    %% application:ensure_all_started(worker_pool),
    %% aerl:start(),
    Wpids = lists:foldr(fun(Env,R) ->
				Local = lists:flatten(Env),
				[awoman:init(Local) | R] end, [],
			WomenList),
    io:format("Woman ~p~n",[length(Wpids)]),
    Mpids = lists:foldr(fun(Env,R) ->
				Local = lists:flatten(Env),
				[aman:init(Local) | R] end, [],
    			  MenList),
    io:format("Man ~p~n",[length(Mpids)]),
    %io:format("Wid ~p~n",[Wid]),
    L1 = lists:zip(WomenList,Wpids),
    %%io:format("L1 ~p~n",[L1]),
    lists:foreach(fun({[Env,_],P}) ->
			  ok = aerl:register(P,Env) end, L1),
    %%Mid = [Id || {Id,_} <- MenList],
    %io:format("Mid ~p~n",[Mid]),
    L2 = lists:zip(MenList,Mpids),
    %%io:format("L2 ~p~n",[L2]),
    lists:foreach(fun({[Env,_],P}) ->
			  ok = aerl:register(P,Env) end, L2),
    [Wpids,Mpids].

start(WomenList,MenList,Wpids,Mpids) ->
    %% Start processes
    [X ! start || X <- Mpids],
    [X ! start || X <- Wpids],
    log(WomenList,MenList).

log(WomenList,MenList) ->
    receive
    after 1000 ->
	    State = ets:tab2list(?MODULE),
	    io:format("Matching size: ~p~n",[length(State)]),
	    case length(State) == length(MenList) of %%andalso stable:check(State,WomenList,MenList) == true of
		true ->
		    proplists:delete(single,State);
		false ->
		    log(WomenList,MenList)
	    end
    end.

output(State,W,M) ->
    %%io:format("Matching: ~p~n",[State]),
    io:format("Matching Size=~p~n",[length(State)]),
    io:format("Stable=~p~n",[stable:check(State,W,M)]).
