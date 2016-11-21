-module(awoman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

init(Env,Prefs) ->
    spawn(?MODULE, start,[Env,Prefs]).

start(Env,Prefs) ->
    aerl_env:initEnv(Env),
    aerl:setAtt(prefs,Prefs),
    aerl:setAtt(pstatus,none),
    receive
	start -> loop(Prefs)
    end.

loop(Prefs) ->
    [Id,Partner,PStatus] = aerl:getAtts([id,partner,pstatus]),
%%    from("id in this.prefs"),
    from("sex = _man"),
    receive
    	{Ref, propose, Man, MStatus} ->
	    %%io:format("~p receives propose ~p ~n",[Id,Man]),
    	    case bof(Prefs, Partner, PStatus, Man, MStatus) of
    		true ->
    		    to("id = $Man") ! {Ref, yes, Id},
    		    %%from("id = $Man"),
    		    receive
    			{Ref, confirm,Man} ->
			    log(Man,Id),
    			    to("id = $Partner") ! {no, Id},
    			    aerl:setAtts([{partner,Man},{pstatus,MStatus}]);
    			{Ref, sorry, _} ->
			    ok
    		    end;
    		false ->
    		    to("id = $Man") ! {Ref, no, Id}
    	    end,
    	    loop(Prefs)
    end.




bof(_, none, _, _, _) -> true;
bof([], _, _, _, _) -> false;
bof([H|T], Partner, PStatus, Man, MStatus) ->
  case H of
    Man -> true;
    Partner -> false;
    List when is_list(List) ->
	  case {lists:member(Man,List), lists:member(Partner,List)} of
	      {true, true} -> MStatus >= PStatus;
	      {true, false} -> true;
	      {false, true} -> false;
	      _ ->
		  bof(T, Partner, PStatus, Man, MStatus)
	  end;
      _ -> bof(T, Partner, PStatus, Man, MStatus)
  end.

log(Partner,Id) ->
    %%io:format("woman ~p is engaged to ~p ~n",[Id,Partner]),
    ets:insert(asmp,{Partner,Id}).


better(E,H) ->
    lists:reverse(cut(E,lists:reverse(H))).

cut(_,[]) -> [];
cut(E,[E|T]) ->
    [E|T];
cut(E,[H|T]) when is_list(H) ->
    case lists:member(E,H) of
	true ->
	    [H|T];
	false ->
	    cut(E,T)
    end;
cut(E,[H|T]) ->
    cut(E,T).


prior(none,Prefs) ->
    0;
prior(E,Prefs) ->
    prior(E,Prefs,0).

prior(E,[E|_T],Acc) ->
    Acc;
prior(E,[H|T],Acc) when is_list(H) ->
    case lists:member(E,H) of
	true ->
	    Acc;
	false ->
	    prior(E,T,Acc+1)
    end;
prior(E,[_H|T],Acc) ->
    prior(E,T,Acc+1).



	%% {propose, Ref, Man, MStatus} when MStatus > 1,Man == Old ->
	%%     case MStatus >= PStatus of
	%% 	true ->
	%% 	    log(Man,Id),
	%% 	    to("id = $Man") ! {Ref, yes,Id},
	%% 	    to("id = $Partner") ! {no,Id},
	%% 	    aerl:setAtt(old,Partner),
	%% 	    aerl:setAtts([{partner,Man},{pstatus,MStatus}]);
	%% 	false ->
	%% 	    to("id = $Man") ! {Ref, no, Id}
	%%     end,
	%%     loop(Prefs);
	%% {propose, Ref, Man, MStatus} when MStatus > 1 ->
	%%     case prior(Man,Prefs) == prior(Partner,Prefs) + MStatus - 1 of
	%% 	true ->
	%% 	    log(Man,Id),
	%% 	    to("id = $Man") ! {Ref, yes,Id},
	%% 	    to("id = $Partner") ! {restart,Id},
	%% 	    aerl:setAtt(old,Partner),
	%% 	    aerl:setAtts([{partner,Man},{pstatus,MStatus}]);
	%% 	false ->
	%% 	    to("id = $Man") ! {Ref, no,Id}
	%%     end,
	%%     loop(Prefs);

%% loop(Prefs) ->
%%     [Id,Partner,PStatus,Old] = aerl:getAtts([id,partner,pstatus,old]),
%%     from("sex = _man"),
%%     receive
%% 	{propose, Ref, Man, MStatus} ->
%% 	    case bof(Prefs, Partner, PStatus, Man, MStatus) of
%% 		true ->
%% 		    to("id = $Man") ! {Ref, yes, Id},
%% 		    from("id = $Man"),
%% 		    receive
%% 			{Ref, confirm,Man} ->
%% 			    log(Man,Id),
%% 			    to("id = $Partner") ! {no, Id},
%% 			    aerl:setAtts([{partner,Man},{pstatus,MStatus}]);
%% 			{Ref, sorry,Man} -> ok
%% 		    end;
%% 		false ->
%% 		    to("id = $Man") ! {Ref, no, Id}
%% 	    end,
%% 	    loop(Prefs);
%% 	_ -> loop(Prefs)
%%     end.
