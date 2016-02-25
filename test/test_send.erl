-module(test_send).
-compile({parse_transform,aerl_trans}).

-compile(export_all).
-define(NUM_PROCS, 10).

start() ->
    aerlang:start(),
    io:format("Create Receiving Processes~n"),
    Pids = create_process(?NUM_PROCS,[]),
    timer:sleep(1000),


    Predicate = "X < 5",
    io:format("Create Sending Process, with Predicate ~p~n",[Predicate]),
    Pid = spawn(fun() ->
			sender(Predicate) end),


    io:format("Start sending ...~n"),
    Pid ! start_send,

    timer:sleep(2000),
    io:format("Kill all processes ...~n"),
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),
    aerlang:stop().

create_process(0,Pids) ->
    Pids;
create_process(Num,Pids) ->
    Pid = spawn(fun() ->
			receiver(Num) end),
    create_process(Num-1,[Pid | Pids]).

% sender code
sender(Predicate) ->
    Key = ?NUM_PROCS*2,
    Env = env_init(),
    aerlang:register(Key,Env),
    loop1(Predicate).

loop1(Predicate) ->
    receive
	start_send ->
	    to(Predicate) ! {attribute_send, self()},
	    loop1(Predicate)
    end.


% receiver code
receiver(Key) ->
    Env = env_init(),
    aerlang:register(Key,Env),
    io:format("~p has ~p~n",[self(),aerlang:get_env_by_key(Key)]),
    loop2().

loop2() ->
    receive
	{attribute_send, From} ->
	    io:format("~p whose environment ~p receives message from ~p~n",[self(),aerlang:get_env_by_pid(self()),From]),
	     loop2();
	 Other ->
	    io:format("~p receives ~p~n",[self(),Other])
end.

env_init() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    X = random:uniform(10),
    Y = random:uniform(10),
    [{'X',X},{'Y',Y}].


%env_init(Id) ->
%    L1 = [red, green, blue, black],
%    L2 = [explorer, rescue, charger, helping],
%    [{'Id', Id}, {'Color', random_element(L1)}, {'Role', random_element(L2)}, {'Battery', random:uniform(100)}].

%random_element(L) ->
%    Index = random:uniform(length(L)),
%    lists:nth(Index,L).
