-module(eman).
-compile(export_all).

% AbC code for Man
% M := [partner:=top[preference];preference:=preference.tail](propose,this.mid)@(wid=partner).(x=invalid)(x).M

init(Env) ->
    spawn(fun() ->
		  start(Env) end).
start(Env) ->
    register(maps:get(id,Env),self()),
    io:format("Man ~p start!~n",[maps:get(id,Env)]),
    man(Env).

man(Env) ->
    [H|T] = maps:get(prefs,Env),
    NEnv = Env#{partner := H, prefs:=T},
    H ! {propose, maps:get(id, NEnv)},
    io:format("Man ~p proposed to Woman ~p~n",[maps:get(id,NEnv),maps:get(partner,NEnv)]),
    receive
	no ->
	    man(NEnv)
    end.
