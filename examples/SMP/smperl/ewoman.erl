-module(ewoman).
-compile(export_all).

% AbC code for woman
% W:= (x=propose)(x,y).(<BOF(this.partner,y)>[this.ex:=this.partner;this.partner:=y](invalid)@(mid=this.ex).0
% + <not BOF(this.partner,y)>(invalid)@(mid=y).0).W

init(Env) ->
    spawn(fun() ->
		  start(Env) end).

start(Env) ->
    register(maps:get(id,Env),self()),
    io:format("Woman ~p start!~n",[maps:get(id,Env)]),
    woman(Env).

woman(Env) ->
    receive
	{propose,Y} ->
	    #{prefs := L, partner := Current, id := Id} = Env,
	    case bof(L, Current, Y) of
		true ->
		    case Current > 0 of
			true -> io:format("Woman ~p reject Man ~p~n",[Id,Current]),
				Current ! no;
			false -> ok
		    end,
		    io:format("Woman ~p has partner ~p~n",[maps:get(id,Env),Y]),
		    woman(Env#{partner:=Y});
		false ->
		    io:format("Woman ~p reject Man ~p~n",[Id,Y]),
		    Y ! no,
		    woman(Env)
	    end
    end.


bof(_, -1, _) -> true;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
    _ -> bof(T, Partner, Y)
  end.
