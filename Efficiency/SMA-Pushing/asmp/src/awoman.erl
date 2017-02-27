-module(awoman).
-compile({parse_transform,aerl_trans_push}).
-compile(export_all).

init(Env) ->
    spawn(?MODULE, start,[Env]).

start(Env) ->
    aerl:initEnv(Env),
    Id = aerl:getAtt(id),
    Prefs = aerl:getAtt(prefs),
    aerl:restrict(prefs),
    %%io:format("~p has ~p and ~p ~n",[Id,self(),Prefs]),
    receive
	start ->
	    loop(Prefs,Id,none,[])
    end.

loop(Prefs,Id,P,PA) ->
    from("bof($Prefs,$PA,wealth,body)"),
    receive
	{propose,M,MA} ->
	    %%io:format("~p say yes to ~p with ~p~n",[Id,M,Atts]),
	    to("id=$M") ! {yes,Id},
	    from("id=$M"),
	    receive
		confirm ->
		    to("id=$P") ! goodbye,
		    engaged(M,Id),
		    loop(Prefs,Id,M,MA);
		busy ->
		    loop(Prefs,Id,P,PA)
	    end
    end.

engaged(Partner,Id) ->
    ets:insert(asmp,{Partner,Id}).
