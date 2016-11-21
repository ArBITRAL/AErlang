-module(aman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

init(Env,Prefs) ->
    spawn(?MODULE, start, [Env,Prefs]).

start(Env,Prefs) ->
    aerl_env:initEnv(Env),
    receive
	start -> loop(Prefs)
    end.

loop([]) ->
    io:format("Alone man is me! ~n"),
    ok;
loop([H|T]) ->
    Id = aerl:getAtt(id),
    to("id = $H") ! {propose, Id},
    from("sex = _woman"),
    receive
	{no,H} ->
	    loop(T)
    end.
