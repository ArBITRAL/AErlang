-module(pman).
-compile(export_all).
-record(state,{id,partner,sex,prefs}).

init(Id,Prefs) ->
    spawn(?MODULE, start,[Id,Prefs]).

start(Id,Prefs) ->
    State = #state{id = Id, sex = man, partner = none, prefs = Prefs},
    receive
	start -> loop(State)
    end.

loop(#state{prefs=[]}) ->
    ok;
%% propose and wait for response
loop(State=#state{prefs=[H|T],id=Id}) ->
    global:whereis_name(H) ! {propose,Id},
    receive
	{no, H} ->
	    loop(State#state{prefs=T})
    end.
