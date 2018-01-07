-module(aman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

init(Env,Prefs) ->
    spawn(?MODULE, start, [Env,Prefs]).

start(Env,Prefs) ->
    aerl_env:initEnv(Env),
    aerl:setAtt(prefs,Prefs),
    aerl:setAtt(candidate,[]),
    receive
	start -> loop(Prefs,0)
    end.

loop(_,10) ->
    global:whereis_name(log) ! stop,
    io:format("Man ~p is alone after ~p try~n",[aerl:getAtt(id),9]);
loop([],Status) ->
    Prefs = aerl:getAtt(prefs),
    aerl:setAtt(partner,none),
    loop(Prefs,Status+1);
loop([H|T],Status) ->
    aerl:setAtt(candidate,[]),
    [Id] = aerl:getAtts([id]),
    Ref = make_ref(),
    Count = to_c("id in $H") ! {Ref, propose, Id, Status},
    from("sex = _woman",Count),
    receive
    	{Ref, yes, Wid} ->
    	    case aerl:getAtt(partner) of
    		none ->
		    %%io:format("~p confirm to ~p~n",[Id,Wid]),
    		    to("id = $Wid") ! {Ref, confirm,Id},
    		    aerl:setAtt(partner,Wid);

    		_ ->
    		    to("id = $Wid") ! {Ref, sorry,Id},
    		    Try = aerl:getAtt(candidate),
    		    aerl:setAtt(candidate,[Wid|Try])
    	    end;
    	{Ref, no, _} -> fine
    end,
    Partner = aerl:getAtt(partner),
    case Partner of
    	none ->
    	    loop(T,Status);
    	_ ->
    	    receive
    		{no, Partner} ->
    		    aerl:setAtt(partner,none),
    		    Try = aerl:getAtt(candidate),
    		    loop(Try++T,Status)
    	    end
    end.
