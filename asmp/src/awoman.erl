-module(awoman).
-compile({parse_transform,aerl_trans}).
-export([init/1,start/1]).

init(Env) ->
    spawn(?MODULE, start, [Env]).

start(Env) ->
    aerl:initEnv(Env),
    Id = aerl:getAtt(id),
    Prefs = aerl:getAtt(prefs),
    aerl:restrict(prefs),
    receive
	start ->
	    loop(Prefs, Id, none, [])
    end.

loop(Prefs, Id, P, PA) ->
    from("bof($Prefs, $PA, wealth, body)"),
    receive
	{propose, M, [W,B]=MA} ->
	    to("id = $M") ! {yes, Id},
	    from("id = $M"),
	    receive
		confirm ->
		    to("id = $P") ! goodbye,
		    engaged(M, Id),
		    loop(Prefs, Id, M, MA);
		busy  ->
		    loop(Prefs, Id, P, PA)
	    end
    end.

engaged(Partner, Id) -> ets:insert(asmp, {Partner, Id}).

count_rec(0) -> ok;
count_rec(X) when X > 0 ->
    ets:update_counter(recv,num,X);
count_rec(X) ->
    io:format("What is this ~p ~n",[X]).
