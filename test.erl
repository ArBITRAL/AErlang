%% TEST AErlang
-module(test).
-compile({parse_transform, aerl_with}).

-export([start/1]).

start(ProcessesCount) ->
    aerlang:start(),
    Pids = create_processes(ProcessesCount),
%    io:format("All Processes ~p~n",[Pids]),
    Pred = "Battery > 80 or Color = red",
%    Pred = "tt",
    Msg = {ping, self()},
    Env = env_init(robot,red,research,90),
    aerlang:register(9999,self(),Env),

    {Time,_V} = timer:tc(aerlang,send,[Msg,Pred]),
    io:format("Time to send ~p ms~n",[Time/1000]),
    io:format("shutdown all~n"),
    [exit(Pid,kill) || Pid <- Pids],
    ok.

env_init(Name,Color,Role,Battery) ->
    [{'Name', Name}, {'Color', Color}, {'Role', Role}, {'Battery', Battery}].

create_process(Key,Name,Color,Role,Battery) ->
    Env = env_init(Name,Color,Role,Battery),
    Pid = spawn(fun() ->
			loop(Env) end),

    aerlang:register(Key,Pid,Env),
%   io:format("~p has ~p~n",[Pid,aerlang:get_env(Key)]),
    Pid.

loop(_State) ->
    with("ff"),
    receive
	{ping, _From}
	   ->
	    io:format("~p has key ~p recives from ~p has environment ~p~n",[self(),aerlang:find_key(self()),_From, aerlang:get_env_by_pid(_From)])
    after 3000 ->
	    timeout
    end.

create_processes(ProcessesCount) ->
   register(rand_gen, spawn(fun() -> rand(os:timestamp()) end)),
   create_processes(ProcessesCount,[]).

create_processes(0,Pids) ->
    unregister(rand_gen),
    Pids;
create_processes(Num,Pids) ->
    Rand = randint(100),
    Pid = create_process(Num,Num,c,r,Rand),
    create_processes(Num-1,[Pid | Pids]).


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
