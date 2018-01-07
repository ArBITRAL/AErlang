-module(awoman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

% AbC code for woman
% W:= (x=propose)(x,y).(<BOF(this.partner,y)>[this.ex:=this.partner;this.partner:=y](invalid)@(mid=this.ex).0
% + <not BOF(this.partner,y)>(invalid)@(mid=y).0).W


init(Env) ->
    spawn(fun() ->
		  start(Env) end).

start(Env) ->
    %% register itself
    aerl:register(Env),
    io:format("Woman <~p> ~p start!~n",[self(),get()]),
    woman().

woman() ->
    [Id,Partner] = aerl:getAtts([id,partner]),
    io:format("Woman ~p has current partner ~p~n",[Id,Partner]),
    from("tt"),
    receive
	{propose,Y} ->
	    [L,Current,Id]=aerl:getAtts([prefs,partner,id]),
	    io:format("Woman ~p is comparing Man ~p vs Man ~p,~n",[Id,Current,Y]),
	    case bof(L, Current, Y) of
		true ->
		    aerl:setAtts([{ex,Current},{partner,Y}]),
		    io:format("Woman ~p is rejecting Man ~p,~n",[Id,Current]),
		    to("id = this.ex") ! no,
		    to("id = this.partner") ! yes,
		    woman();
		false ->
		    to("id = $Y") ! no,
		    io:format("Woman ~p reject Man ~p~n",[Id,Y]),
		    woman()
	    end

    end.


bof(_, -1, _) -> true;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
    _ -> bof(T, Partner, Y)
  end.
