-module(awoman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

init(Env,Prefs) ->
    spawn(?MODULE, start,[Env,Prefs]).

start(Env,Prefs) ->
    aerl_env:initEnv(Env),
    receive
	start -> loop(Prefs)
    end.

loop(Prefs) ->
    [Id,Partner] = aerl:getAtts([id,partner]),
    from("sex = _man"),
    receive
    	{propose, Man} ->
    	    case bof(Prefs,Partner,Man) of
    	    	true ->
		    %%global:whereis_name(log) ! {Partner,Id},
		    log(Man,Id),
		    to("id = $Partner") ! {no, Id},
		    aerl:setAtt(partner,Man),
    		    loop(Prefs);
    		false ->
		    to("id = $Man") ! {no, Id},
    		    loop(Prefs)
    	    end
    end.

bof(_, none, _) -> true;
bof([], _, _) -> false;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
      _ -> bof(T, Partner, Y)
  end.

log(Partner,Id) ->
    ets:insert(asmp,{Partner,Id}).
