-module(aman).
-compile({parse_transform,aerl_trans}).
-export([init/1,start/1]).

init(Env) ->
    spawn(?MODULE, start, [Env]).

start(Env) ->
    aerl_env:initEnv(Env),
    aerl:newAtt(partner, none),
    [Id, Prefs] = aerl:getAtt([id, prefs]),
    Atts = aerl:getAtt([wealth, body]),
    aerl:restrict(prefs),
    receive
	start -> loop(Prefs, Id, Atts)
    end.

loop([], Id, _) ->
    single(Id);
loop([H | T] = Prefs, Id, Atts) ->
    Cnt = to(H) ! {propose, Id, Atts},
    from(H, Cnt),
    receive
	{yes, W} ->
	    case aerl:getAtt(partner) of
		none ->
		    to("id = $W") ! confirm,
		    aerl:setAtt(partner, W);
		_ ->
		    to("id = $W") ! busy
	    end
    end,
    case aerl:getAtt(partner) of
    	none -> loop(T, Id, Atts);
    	Partner -> from("id = this.partner"),
    	     receive
    		 goodbye ->
		     aerl:setAtt(partner, none),
		     loop(Prefs, Id, Atts)
    	     end
    end.

single(Id) -> ets:insert(asmp, {single, Id}).
count_service(0) -> ok;
count_service(X) when X > 0 ->
    ets:update_counter(service,num,X);
count_service(X) ->
    io:format("What is this ~p ~n",[X]).

count_rec(0) -> ok;
count_rec(X) when X > 0 ->
    ets:update_counter(recv,num,X);
count_rec(X) ->
    io:format("What is this ~p ~n",[X]).
