-module(broadcast).

-compile({parse_transform,aerl_trans}).
-compile(export_all).

%% Protocol

%% Ps: Predicate of sender
%% message = {Ps, Msg, Es} where Es is the environment of Sender
%% receiver checks if eval(Ps, Ev) and eval(Pr,Es)

start() ->
    aerlang:start(broadcast),
    Pid = spawn(fun() -> process1() end),
    Env = #{group => c, role => explorer, battery => 80},
    aerlang:register(Env),
    Ps = "role = helper and group = a",
    Msg = qry,
    to(Ps) ! {Msg, Env},
    timer:sleep(5000),
    io:format("resend ~n"),
    to(Ps) ! {Msg, Env},
    Pid.

process1() ->
    Env = #{group => a, role => helper, battery => 50},
    aerlang:register(Env),
    Pr = "group = c and role =/= this.role",
    from(Pr),
    receive
    	{Msg,V} ->
    		io:format("Receive Message ~p with values ~p~n",[Msg,V])
    end.
