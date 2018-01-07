-module(awoman).
-compile({parse_transform,aerl_trans_push}).
-compile(export_all).

init(Env) ->
    spawn(?MODULE, start,[Env]).

start(Env) ->
    aerl:initEnv(Env),
    aerl:restrict(prefs),
    Id = aerl:getAtt(id),
    Prefs = aerl:getAtt(prefs),
    %%io:format("~p has ~p and ~p ~n",[Id,self(),Prefs]),
    receive
	start ->
	    woman(Prefs,Id,none)
    end.

woman(Prefs,Id,P) ->
    from("bof($Prefs,$P,id)"),
    receive
	{propose,M} ->
	    to("id=$M") ! {yes,Id},
	    from("id=$M"),
	    receive
		confirm ->
		    to("id=$P") ! goodbye,
		    engaged(M,Id),
		    woman(Prefs,Id,M);
	        busy ->
		    woman(Prefs,Id,P)
	    end
    end.


engaged(Partner,Id) ->
    ets:insert(asmp,{Partner,Id}).
