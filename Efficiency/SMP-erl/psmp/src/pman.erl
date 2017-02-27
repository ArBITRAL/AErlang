-module(pman).
-compile(export_all).
-record(state,{id,partner,prefs}).

init(Env) ->
    spawn(?MODULE, start,[Env]).

start(Env) ->
    [{id,Id},{prefs,Prefs}] = Env,
    State = #state{id = Id, partner = none, prefs = Prefs},
    %%io:format("man ~p has ~p~n",[Id,Prefs]),
    receive
	start -> man(State)
    end.

man(#state{prefs=[],id=Id}) ->
    single(Id);
%% propose and wait for response
man(State=#state{prefs=Prefs,id=Id,partner=Part}) ->
    [H|T] = Prefs,
    Ref = make_ref(),

    %% proposing phase
    Count = [global:whereis_name(X) ! {Ref,propose,Id} || X <- H],
    count_msg(length(Count)),
    Partner = receive_count(Id,length(Count),Part,Ref),

    %% watiting phase
    case Partner of
	none ->
	    man(State#state{prefs=T});
	_ ->
	    receive
		{goodbye,Partner} ->
		    man(State#state{prefs=Prefs,partner=none})
	    end
    end.

receive_count(_,0,Part,_) -> Part;
receive_count(Id,Count,Part,Ref) ->
    receive
	{Ref,yes,Wid} ->
	    if Part == none ->
		    %%io:format("~p engaged to ~p~n",[Id,Wid]),
		    global:whereis_name(Wid) ! {Ref,confirm,Id},
		    count_msg(1),
		    receive_count(Id,Count-1,Wid,Ref);
	       true ->
		    global:whereis_name(Wid) ! {Ref,busy,Id},
		    count_msg(1),
		    receive_count(Id,Count-1,Part,Ref)
	    end;
	{Ref,sorry,_} ->
	        receive_count(Id,Count-1,Part,Ref)
    end.

single(Id) ->
    ets:insert(psmp,{single,Id}).


count_msg(0) -> ok;
count_msg(X) when X > 0 ->
    ets:update_counter(message,num,X).
