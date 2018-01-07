-module(pwoman).
-compile(export_all).
-record(state, {id, partner, sex, pstatus, prefs}).

init(Id,Prefs) ->
    spawn(?MODULE,start, [Id,Prefs]).

start(Id,Prefs) ->
    State = #state{id = Id, partner = none, sex = woman, prefs = Prefs},
    receive
	start -> loop(State)
    end.

loop(State=#state{prefs=List,id=Id,partner = Partner}) ->
    receive
	{propose, Man}  ->
	    case bof(List,Partner,Man) of
		true ->
		    log(Man,Id),
		    case Partner =/= none of
			true ->
			     global:whereis_name(Partner) ! {no, Id};
			false -> ok
		    end,
		    loop(State#state{partner=Man});
		false ->
		    global:whereis_name(Man) ! {no, Id},
		    loop(State)
	    end
    end.

bof(_, none, _) -> true;
bof([], _ ,_) -> false;
bof([H|T], Partner, Y) ->
    case H of
	Y -> true;
	Partner -> false;
	_ -> bof(T, Partner, Y)
    end.

log(Partner,Id) ->
    %%io:format("woman ~p is engaged to ~p ~n",[Id,Partner]),
    ets:insert(psmp,{Partner,Id}).
