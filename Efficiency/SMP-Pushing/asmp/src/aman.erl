-module(aman).
-compile({parse_transform,aerl_trans_push}).
-compile(export_all).

init(Env) ->
    spawn(?MODULE, start, [Env]).


start(Env) ->
    aerl_env:initEnv(Env),
    aerl:restrict(prefs),
    aerl:newAtt(partner,none),
    [Id,Prefs] = aerl:getAtt([id,prefs]),
    receive
	start -> loop(Prefs,Id)
    end.

loop([],Id) ->
    single(Id);
loop(Prefs,Id) ->
    [H|T]=Prefs,
    Cnt=to("id in $H")!{propose,Id},
    from("id in $H",Cnt),
    receive
	{yes,W} ->
	    case aerl:getAtt(partner) of
		none ->
		    to("id=$W") ! confirm,
		    aerl:setAtt(partner,W);
		_ ->
		    to("id=$W") ! busy
	    end
    end,
    case aerl:getAtt(partner) of
    	none -> loop(T,Id);
    	_ -> from("id=this.partner"),
    	     receive
    		 goodbye ->
    		     aerl:setAtt(partner,none),
		     loop(Prefs,Id)
    	     end
    end.

single(Id) ->
    ets:insert(asmp,{single,Id}).
