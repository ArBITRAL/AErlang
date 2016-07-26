-module(awoman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

% AbC code for woman
% W:= (x=propose)(x,y).(<BOF(this.partner,y)>[this.ex:=this.partner;this.partner:=y](invalid)@(mid=this.ex).0
% + <not BOF(this.partner,y)>(invalid)@(mid=y).0).W
%

% using map to store environment

init(Env) ->
    spawn(fun() ->
		  start(Env) end).

start(Env) ->
    aerl:register(Env),
    io:format("Woman <~p> ~p start!~n",[self(),maps:get(id,Env)]),
    loop(Env).

loop(Env) ->
    io:format("Woman ~p has current partner ~p with score ~p~n",[maps:get(id,Env),maps:get(partner,Env),maps:get(wpartner,Env)]),
    #{prefs := L, partner := Partner, id := Id, wpartner := Param} = Env,
    from("x = propose"),
    receive
	{X,Man,W,B} ->
	    case bof(L, Param, W, B) of
		{true,Score} ->
%		   io:format("[trade up] Woman ~p replaces Man ~p by Man ~p with score ~p,~n",[Id,Partner,Man,Score]),
		    timer:sleep(2000),
		     to("id = $Man") ! {{yes, Id},Env},
		     io:format("Woman ~p says yes to ~p~n",[Id,Man]),
		     from("X1 = confirm or X1 = sorry"),
		     receive
			 {X1} ->
			     case X1 of
				 confirm ->
				     NEnv = Env#{ex := Partner, partner := Man, wpartner := Score},
				     io:format("Woman ~p is engaged to man ~p~n",[Id,Man]),
				     to("id = this.ex") ! {{no, Id},NEnv},
				     loop(NEnv);
				 sorry ->
				     loop(Env)
			     end
		    end;
		{false,_} ->
		    to("id = $Man") ! {{no, Id},Env},
		    io:format("Woman ~p rejects Man ~p~n",[Id,Man]),
		    loop(Env)
	    end
    end.


%bof(_, -1, _ , _) -> {true,-1};
bof(L, Param, W, B) ->
  #{wealth := X, body := Y} = L,
  Score = case {X,Y} of
      {W,B} -> 3;
      {W,_} -> 2;
      {_,B} -> 1;
      _Other ->0
  end,
  {Score >= Param,Score}.
