-module(aman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

% AbC code for Man M :=
% [partner:=top[preference];preference:=preference.tail](propose,this.mid)@(wid=partner).(x=invalid)(x).M

init(Env) ->
    spawn(fun() ->
		  start(Env) end).
start(Env) ->
    aerl:register(Env),
    io:format("Man <~p> ~p start!~n",[self(),maps:get(id,Env)]),
    man().

man() ->
    [H|T] = aerl:getAtt(prefs),
    aerl:setAtt(partner,H),
    aerl:setAtt(prefs,T),
    Id = aerl:getAtt(id),
    to("id = this.partner") ! {propose, Id},
    io:format("Man ~p proposed to Woman ~p~n",[Id,H]),
    from("tt"),
    receive
    	yes ->
    	    from("tt"),
    	    receive
    		no -> io:format("Man ~p get rejected by partner and try again~n",[Id]),
		      man()
    	    end;
    	no -> io:format("Man ~p try again~n",[Id]),man()
    end.
