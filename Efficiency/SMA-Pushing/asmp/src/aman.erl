-module(aman).
-compile({parse_transform,aerl_trans_push}).
-compile(export_all).

init(Env) ->
    spawn(?MODULE, start, [Env]).

start(Env) ->
    aerl_env:initEnv(Env),
    aerl:newAtt(partner,none),
    [Id,Prefs] = aerl:getAtt([id,prefs]),
    Atts = aerl:getAtt([wealth,body]),
    aerl:restrict(prefs),
    %%io:format("~p has ~p and ~p~n",[Id,P,PAtts]),
    receive
	start -> loop(Prefs,Id,Atts)
    end.

loop([],Id,_) ->
    single(Id);
loop(Prefs,Id,Atts) ->
    [H|T]=Prefs,
    Cnt=to(H)!{propose,Id,Atts},
    from(H,Cnt),
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
    	none -> loop(T,Id,Atts);
    	_ -> from("id=this.partner"),
    	     receive
    		 goodbye ->
    		     aerl:setAtt(partner,none),
		     loop(Prefs,Id,Atts)
    	     end
    end.

single(Id) ->
    ets:insert(asmp,{single,Id}).
