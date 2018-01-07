-module(psmp).
-compile(export_all).

print(X) ->
    io:format("~p~n",[X]).

run(_,_,[]) -> init:stop();
run(N,PATH,[H|T]) ->
    Start1 = os:timestamp(),
    Path = PATH++H,
    io:format("\n===============================================\n"),
    io:format("Solving problem in ~p",[Path]),
    io:format("\n===============================================\n"),
    %% Read data in form of attributes, and translate into preference lists
    transinput:start(Path),
    W = read1:wread(Path++"/women_prefs.list"),
    M = read1:mread(Path++"/men_prefs.list"),
     %% print(M),
     %% print(W),
    %% Data from X10 format
    %% W = read:wreadlines(Path++"/women.list"),
    %% M = read:mreadlines(Path++"/men.list"),
    io:format("IO time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start1)/1000]),
    ok = execute(N,W,M),
    run(N,PATH,T).

execute(0,_,_) -> ok;
execute(N,WomenList,MenList) ->
    io:format("~n ---- Run ~p: ----~n",[N]),
    ets:new(?MODULE,[named_table,public,{write_concurrency,true}, {read_concurrency,false},{keypos,2}]),
    ets:new(message,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:insert(message,{num,0}),
    Start2 = os:timestamp(),
    [Wid,Mid,Wpids,Mpids] = init(WomenList,MenList),
    io:format("Init time=~p miliseconds~n",[timer:now_diff(os:timestamp(), Start2)/1000]),
    Start3 = os:timestamp(),
    Matching = start(WomenList,MenList,Wpids,Mpids),
    io:format("Computation time=~p seconds~n",[timer:now_diff(os:timestamp(), Start3)/1000000]),
    io:format("Number of Messages=~p~n",[ets:lookup_element(message,num,2)]),
    Start4 = os:timestamp(),
    output(Matching,WomenList,MenList),
    io:format("Stability check time=~p seconds~n",[timer:now_diff(os:timestamp(), Start4)/1000000]),
    %% Clean up
    ok = clean_up(Wid,Mid,Wpids,Mpids),
    execute(N-1,WomenList,MenList).

init(WomenList,MenList) ->
    Wpids = lists:foldr(fun(Env,R) ->
    				Local = lists:flatten(Env),
    				[pwoman:init(Local) | R] end, [],
    			WomenList),
    io:format("Woman ~p~n",[length(Wpids)]),
    Mpids = lists:foldr(fun(Env,R) ->
    				Local = lists:flatten(Env),
    				[pman:init(Local) | R] end, [],
    			  MenList),
    io:format("Man ~p~n",[length(Mpids)]),

    Wid = [Id || [{_,Id},_] <- WomenList],
    Mid = [Id || [{_,Id},_] <- MenList],
    L1 = lists:zip(Wid,Wpids),
    L2 = lists:zip(Mid,Mpids),

    lists:foreach(fun({Id,P}) ->
    			  yes = global:register_name(Id,P) end, L1),

    lists:foreach(fun({Id,P}) ->
    			  yes = global:register_name(Id,P) end, L2),
    [Wid,Mid,Wpids,Mpids].

clean_up(Wid,Mid,Wpids,Mpids) ->
    L1 = lists:zip(Wid,Wpids),
    L2 = lists:zip(Mid,Mpids),
    lists:foreach(fun({Id,P}) ->
			  exit(P,kill),
    			  ok=global:unregister_name(Id)
		  end, L1),

    lists:foreach(fun({Id,P}) ->
			  exit(P,kill),
    			  ok=global:unregister_name(Id)
		  end, L2),
    true = ets:delete(?MODULE),
    true = ets:delete(message),
    ok.

start(WomenList,MenList,Wpids,Mpids) ->
    [X ! start || X <- Wpids],
    [X ! start || X <- Mpids],
    log(WomenList,MenList).

log(WomenList,MenList) ->
    receive
    after
	10 ->
	    State = ets:tab2list(?MODULE),
	    case length(State) == length(MenList) of %%andalso stable:check(State,WomenList,MenList) == true of
		true ->
		    proplists:delete(single,State);
		false ->
		    log(WomenList,MenList)
	    end
    end.

output(Matching,WomenList,MenList) ->
    %%io:format("Matching ~p~n",[Matching]),
    io:format("Matching Size=~p~n",[length(Matching)]),
    io:format("Stable=~p ~n",[stable:check(Matching,WomenList,MenList)]).
