-module(aman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

% AbC code for Man
% M := [partner:=top[preference];preference:=preference.tail](propose,this.mid)@(wid=partner).(x=invalid)(x).M

init(Env) ->
    spawn(fun() ->
		  start(Env) end).
start(Env) ->
    aerl:register(Env),
    io:format("Man <~p> ~p start!~n",[self(),maps:get(id,Env)]),
    man().

man() ->
    [Id,Body,Wealth,[H|T],Refusal]=aerl:getAtts([id,body,wealth,predicate,refusal]),
    case  Refusal of
	[] -> Ps = H;
	_ -> Ps = string:concat(H, refuse_pred(Refusal))
    end,
    io:format("Predicate ~p~n",[Ps]),
    to(Ps) ! {propose, Id, Wealth, Body},
    from("tt"),
    receive
	{yes, Wid} ->
	    from("tt"),
	    receive
		{no, Wid} ->
		    aerl:setAtts([{predicate,T},{refusal,refuse_list(Wid,Refusal)}]),
		    man()
	    end;
	{no,Wid} ->
	    aerl:setAtts([{predicate,T},{refusal,refuse_list(Wid,Refusal)}]),
	    man()
    end.

refuse_list(L1,L2) ->
    L = lists:merge([L1],L2),
    lists:usort(L).

refuse_pred(L) ->
    lists:foldl(fun(X, Sum) -> " and id =/= " ++ atom_to_list(X) ++ Sum end, "", L).
