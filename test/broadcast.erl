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
    Variable = 10,
    Pi = aerlang:eval_pred(Pr,Env),
    F = fun(Foo) ->
	receive
	    {Ps, Msg, V} ->
		case aerlang:check_pred(Ps, Env) andalso aerlang:check_pred(Pi,V) of
		    true ->
			    %V1 = [Y || {_,Y} <- V],
			    self() ! {Msg, V},
			    receive
				{Msg, V2} ->
				    io:format("receive ~p with value ~p ~n", [Msg,V2]);
				    %% Works with recursively call process1();
				done -> ok
			    end;
		    false -> Foo(Foo)
		end
	end
	end,
    F(F).
