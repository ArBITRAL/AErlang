-module(ring).
-compile({parse_transform,aerl_trans_push}).
-compile(export_all).

print(X) ->
    io:format("~p~n",[X]).

run() ->
    %%start aerl
    ok = aerl:start(),
    ok = application:start(os_mon),
    %% random list of predicate in here, file name scenario.txt
    Path = code:priv_dir(asmp),
    Filename = Path ++ "/scenario500",
    {ok, Terms} = file:consult(Filename),
    Preds = proplists:get_value(pred,Terms),
    Envs = proplists:get_value(env,Terms),
   % print(Preds),
   % print(Envs),
    %% measure number of messages
    ets:new(message,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:new(request,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:new(service,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:new(broker,[named_table,public,{write_concurrency,true}, {read_concurrency,false}]),
    ets:insert(request,{num,0}),
    ets:insert(service,{num,0}),
    N=length(Envs),
    %% spawn N processes
    Procs = spawn_procs(N,Envs,Preds),
    RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
    %% wait for all to be done by receving all message of form {done, PId}
    ok=receive_msgs(RMsgs),
%    io:format("My table ~p~n", [ets:tab2list(aerl_sub)]),
    %% trigger them to start scenario
    Start = os:timestamp(),
    send_procs(Procs, {procs, Procs, self()}),
    simulate(1000,Procs,Start),
    %% terminate all process
    ok=receive_msgs(RMsgs),
     true = ets:delete(message),
    ok = aerl:stop().

simulate(Interval,Procs,Start1) ->
    receive
	A -> io:format("recevie ~p~n",[A])
    after Interval ->
	    Memsup = memsup:get_system_memory_data(),
	    Total = proplists:get_value(total_memory,Memsup),
	    Free = proplists:get_value(free_memory,Memsup),
	    Used = Total - Free,
%	    io:format("Number of Messages=~p~n",[ets:lookup_element(message,num,2)]),
	    Message = ets:tab2list(message),
	    Brokertime = ets:tab2list(broker),
	    Sum1 = lists:foldl(fun({_K,V},Sum) -> V + Sum end, 0, Message),
	    Sum2 = lists:foldl(fun({_K,V},Sum) -> V + Sum end, 0, Brokertime),
	    io:format("~n=============================~n"),
	    io:format("avg agent msg so far ~p ~n",[Sum1/length(Message)]),
	    io:format("avg broker serv so far ~p ~n",[Sum2/length(Brokertime)]),
	    io:format("memory usage so far ~p ~n",[Used]),
	    Req = ets:lookup_element(request,num,2),
	    Serv = ets:lookup_element(service,num,2),
	    io:format("Number of filtering agents per request ~p~n",[Serv/Req]),
	    io:format("Computation time=~p seconds~n",[timer:now_diff(os:timestamp(), Start1)/1000000]),
	    send_procs(Procs,send),
	    simulate(Interval,Procs,Start1)
    end.

spawn_procs(N,Envs,Preds) ->
    spawn_procs(N,Envs,Preds,N).

spawn_procs(N,_,_,_Total) when N =< 0 ->
    [];
spawn_procs(N,[Env|Envs],[Pred|Preds],Total) ->
    Parent = self(),
    [spawn_link(fun () -> start(N-1,Parent,Env,Pred,Total) end) | spawn_procs(N-1,Envs,Preds,Total)].

start(Id,Parent,E,Pred,N) ->
    Env=E++[{id,Id}],
    aerl:register(Env),
    aerl_env:initEnv(Env),
    Now = now_to_micro_seconds(os:timestamp()),
    aerl_env:newA(start,Now),
    aerl_env:newA(time,0),
    aerl_env:newA(num_msg,0),
    Parent ! {done, self()},
    receive
	{procs, Procs, ReportTo} ->
	    pinger(Id,Parent,Pred,N)
    end.

%% pinger(0,Parent,Pred,N) ->
%%     Id = 0,
%%     From = (Id - 1 + N) rem N,
%%     To = (Id + 1 + N) rem N,
%%     io:format("~p sends to ~p ~n", [Id, To]),
%%     %to("tt") ! ping,
%%     to("id = $To") ! ping,
%%     from("id=$From"),
%%     %%from("tt"),
%%     receive
%% 	ping ->
%% 	    count_msg(1),
%% 	    io:format("~p receives from ~p ~n", [Id, From]),
%% 	    ok
%% %    after 1000 -> ok
%%     end,
%%     Parent ! {done, self()};

pinger(Id,Parent,Pred,N) ->
    to(Pred) ! {ping,Id},
    recv(Id,Parent,Pred).

recv(Id,Parent,Pred) ->
    from(Pred),
    receive
	{ping,From} ->
	    %io:format("My environment ~p~n",[aerl:getEnv()]),
	    Now = now_to_micro_seconds(os:timestamp()),
	    Start = aerl:getAtt(start),
	    aerl:setAtt(time,Now-Start),
	    Old = aerl:getAtt(num_msg),
	    aerl:setAtt(num_msg,Old+1),
	    to("id=$From") ! {ack,Id},
	    %to("tt") ! ping,
	  %  count_msg(1),
	  %  io:format("~p send ack to ~p ~n", [Id, From]),
	    recv(Id,Parent,Pred);
	{ack, From} ->
	   % count_msg(1),
	  %  io:format("~p received ack from ~p ~n", [Id, From]),
	    recv(Id,Parent,Pred)
    after 0 ->
	    Time = aerl:getAtt(time),
	    Msg = aerl:getAtt(num_msg),
	    Avg = if Msg == 0 -> 0; true -> (Time/Msg)/1000 end,
	    ets:insert(message,{Id,Avg}),
%     	    io:format("~p got ~p msgs in ~p / average time of msg arrive ~p ~n", [Id,Msg,Time/1000,Avg]),
	    receive
		send ->
		    to(Pred) ! {ping,Id},
		    recv(Id,Parent,Pred)
	    end
     	    %% Parent ! {done, self()}
     	    %to("id = $To") ! ping,
    %% 	    %to("tt") ! ping,
    %% 	    io:format("~p sends to ~p ~n", [Id, To])
    end.




send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs([]) ->
	ok;
receive_msgs([M|Ms]) ->
	receive
		M -> receive_msgs(Ms)
	end.




seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.
