-module(test_receive).
-compile({parse_transform,aerl_trans}).

-compile(export_all).
-define(NUM_PROCS, 10).

start() ->
    aerlang:start(),

    Predicate = "Color = red and Battery > 30",
    io:format("Create Receiving Process, with Predicate ~p~n",[Predicate]),
    Pid = spawn(fun() ->
			receiver(Predicate) end),

    timer:sleep(1000),
    io:format("Create Sending Processes~n"),
    Pids = create_process(?NUM_PROCS,[]),
    timer:sleep(1000),


    io:format("Start sending ...~n"),
    Id = ?NUM_PROCS * 2,
    Msg = {start_send, Id},
    [Pid || Pid <- Pids, (Pid ! Msg) =:= Msg],
    timer:sleep(2000).

create_process(0,Pids) ->
    Pids;
create_process(Num,Pids) ->
    Pid = spawn(fun() ->
			sender(Num) end),
    create_process(Num-1,[Pid | Pids]).

% sender code
sender(Key) ->
    Env = env_init(),
    aerlang:register(Key,Env),
    io:format("~p has ~p~n",[self(),aerlang:get_env_by_key(Key)]),
    loop1().

loop1() ->
    receive
	{start_send, Id} ->
	    to("tt") ! {attribute_send, self()},
	    loop1()
    end.


% receiver code
receiver(Predicate) ->
    Key = ?NUM_PROCS * 2,
    Env = env_init(),
    aerlang:register(Key,Env),
    io:format("Receiver ~p has ~p~n",[self(),aerlang:get_env_by_key(Key)]),
    loop2(Predicate).

loop2(Predicate) ->
    from(Predicate),
    receive
	{attribute_send, From} ->
	    io:format("~p receives from ~p whose environment is ~p~n",[self(),From,aerlang:get_env_by_pid(From)]),
	     loop2(Predicate)
    after 1000
	      ->
	    io:format("~p finishes receive~n",[self()])
end.


env_init() ->
    L1 = [red, green, blue, black],
    L2 = [explorer, rescue, charger, helping],
    % for random seed
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    [{'Color', random_element(L1)}, {'Role', random_element(L2)}, {'Battery', random:uniform(100)}].

random_element(L) ->
    Index = random:uniform(length(L)),
    lists:nth(Index,L).
