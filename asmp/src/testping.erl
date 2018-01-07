-module(testping).

-compile(export_all).

print(X) -> io:format("~p~n", [X]).

run() ->
    ok = aerl:start(),
    Path = code:priv_dir(asmp),
    Filename = Path ++ "/scenario.txt",
    {ok, Terms} = file:consult(Filename),
    Preds = proplists:get_value(pred, Terms),
    Envs = proplists:get_value(env, Terms),
    print(Preds),
    print(Envs),
    ets:new(message,
	    [named_table, public, {write_concurrency, true},
	     {read_concurrency, false}]),
    ets:insert(message, {num, 0}),
    N = length(Envs),
    Procs = spawn_procs(N, Envs, Preds),
    Start1 = os:timestamp(),
    RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
    ok = receive_msgs(RMsgs),
    io:format("My table ~p~n", [ets:tab2list(aerl_sub)]),
    send_procs(Procs, {procs, Procs, self()}),
    ok = receive_msgs(RMsgs),
    io:format("Number of Messages=~p~n",
	      [ets:lookup_element(message, num, 2)]),
    io:format("Computation time=~p seconds~n",
	      [timer:now_diff(os:timestamp(), Start1) / 1000000]),
    true = ets:delete(message),
    ok = aerl:stop().

spawn_procs(N, Envs, Preds) ->
    spawn_procs(N, Envs, Preds, N).

spawn_procs(N, _, _, _Total) when N =< 0 -> [];
spawn_procs(N, [Env | Envs], [Pred | Preds], Total) ->
    Parent = self(),
    [spawn_link(fun () ->
			start(N - 1, Parent, Env, Pred, Total)
		end)
     | spawn_procs(N - 1, Envs, Preds, Total)].

start(Id, Parent, E, Pred, N) ->
    Env = E ++ [{id, Id}],
    aerl:register(Env),
    aerl_env:initEnv(Env),
    Parent ! {done, self()},
    receive
      {procs, Procs, ReportTo} -> pinger(Id, Parent, Pred, N)
    end.

pinger(Id, Parent, Pred, N) ->
    C = aerl_check:local_eval_send2(Pred, [], {ping, Id}),
    io:format("~p broadcast to ~p : ~p ~n", [Id, Pred, C]),
    recv(Parent, Pred).

recv(Parent, Pred) ->
    _Rpred86 = aerl_check:local_eval_receive_push(Pred, []),
    _F86 = fun (_Foo86) ->
		   receive
		       {{ping, From}, _Decoration88} ->
			   io:format("Received ~p ~p, check in ~p ~n",[From,_Decoration88,_Rpred86]),
			 case aerl_check:check(_Rpred86, _Decoration88) of
			   true ->
			       count_msg(1),
			       io:format("~p sent to ~p ~n",
					 [aerl:getA(id), From]);
			   false ->
			       aerl_check:aerl_no_reply(_Decoration88),
			       _Foo86(_Foo86)
			 end
		   end
	   end,
    _F86(_F86),
    recv(Parent, Pred).

send_procs([], Msg) -> Msg;
send_procs([P | Ps], Msg) ->
    P ! Msg, send_procs(Ps, Msg).

receive_msgs([]) -> ok;
receive_msgs([M | Ms]) ->
    receive M -> receive_msgs(Ms) end.

count_msg(0) -> ok;
count_msg(X) when X > 0 ->
    ets:update_counter(message, num, X);
count_msg(X) -> io:format("What is this ~p ~n", [X]).
