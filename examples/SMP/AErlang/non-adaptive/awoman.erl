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
    woman().

woman() ->
    [L,Partner,Id,Prior] = aerl:getAtts([prefs,partner,id,prior]),
    io:format("Woman ~p has current partner ~p with score ~p~n",[Id,Partner,Prior]),
    from("$X = propose"),
    receive
	{X,M,W,B} ->
		case bof(L, Prior, W, B) of
		    {true,New} ->
			to("id = this.partner") ! {no, Id},
			to("id = $M") ! {yes, Id},
			aerl:setAtts([{partner,M},{prior,New}]),
			woman();
		    {false,_} ->
			to("id = $M") ! {no, Id},
			woman()
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
