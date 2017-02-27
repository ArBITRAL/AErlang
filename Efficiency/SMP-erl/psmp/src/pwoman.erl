-module(pwoman).
-compile(export_all).
-record(state, {id, partner, prefs}).

init(Env) ->
    spawn(?MODULE, start,[Env]).

start(Env) ->
    [{id,Id},{prefs,Prefs}] = Env,
    State = #state{id = Id, partner = none, prefs = Prefs},
    receive
	start -> woman(State)
    end.

woman(State=#state{prefs=List,id=Id,partner = Partner}) ->
    receive
	{Ref,propose, Man}  ->
	    case bof(List,Partner,Man) of
		true ->
		    global:whereis_name(Man) ! {Ref, yes, Id},
		    count_msg(1),
		    receive
			{Ref,confirm,Man} ->
			    engaged(Man,Id),
			    Pid = global:whereis_name(Partner),
			    if is_pid(Pid) == true ->
				    count_msg(1),
				    Pid ! {goodbye, Id};
			       true -> ok
			    end,
			    woman(State#state{partner=Man});
			{Ref,busy,_} ->
			    woman(State)
		    end;
		false ->
		    global:whereis_name(Man) ! {Ref, sorry, Id},
		    count_msg(1),
		    woman(State)
	    end
    end.

bof(_,none,_) -> true;
bof([],_,_) -> false;
bof([H|T],P,M) ->
    case {lists:member(P,H),lists:member(M,H)} of
	{true,_} -> false;
	{false,true} -> true;
	_ -> bof(T,P,M)
    end.


engaged(Partner,Id) ->
    ets:insert(psmp,{Partner,Id}).

count_msg(0) -> ok;
count_msg(X) when X > 0 ->
    ets:update_counter(message,num,X).
