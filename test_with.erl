%% TEST AErlang
-module(test_with).

-compile({parse_transform, aerl_with}).
-export([start/0]).

start() ->
    aerlang:start(),

    io:format("Create Receiving Processes~n"),
    Predicate = "Color = red",
    Pid = spawn(fun() ->
			loop1(Predicate) end),

    io:format("Create Sending Processes~n"),

    Msg = {ping, self()},
    P1 = create_process(1,robot1,red,research,90),
    P2 = create_process(2,robot2,blue,sleep,30),
    P3 = create_process(3,robot3,red,play,30),
    P4 = create_process(4,robot3,red,writing,100),

    io:format("Start send~n"),
    P1 ! {send, Pid},
    P2 ! {send, Pid},
    P3 ! {send, Pid},
    P4 ! {send, Pid},
   % aerlang:stop(),
    ok.

env_init(Name,Color,Role,Battery) ->
    [{'Name', Name}, {'Color', Color}, {'Role', Role}, {'Battery', Battery}].

create_process(Key,Name,Color,Role,Battery) ->
    Env = env_init(Name,Color,Role,Battery),
    Pid = spawn(fun() ->
			loop() end),

    aerlang:register(Key,Pid,Env),
    timer:sleep(1000),
%    io:format("~p has ~p~n",[Pid,aerlang:get_env_by_pid(Pid)]),
    Pid.

loop() ->
    receive
	{send, Pid} ->
	     Msg = {ping, self()},
	    Pid ! Msg,
	    loop()
end.

loop1(Predicate) ->
    with (Predicate),
    receive
	{ping, _From}
	   ->
                io:format("~p recives from ~p has environment ~p~n",
                          [self(), _From, aerlang:get_env_by_pid(_From)]),
               loop1(Predicate)
    end.


% integer random generator, relies on random module
% state is now()

rand(State) ->
    receive
        {From, N} when is_pid(From) ->
            {Int, NewState} = random:uniform_s(N, State),
            From ! {randgen, Int},
            rand(NewState);
        _Other ->
            rand(State)
    end.

randint(N) ->
    rand_gen ! {self(), N},
    receive
	{randgen, Int} ->
	    Int
    after
	1000 -> {error, timeout}
    end.
