%% In this version, I update ets table in reap_datum function!
%% As a result, I can estimate the time of cache replacement process
%% That is, the communication time + ets table update time.
-module(cache).

-export([test/0]).
%% Utility functions for clients contacting the cache manager.
-export([get_cached_data/2, get_stats/1]).
-export([get_datum_pid/2]).

%% Cache Manager functions.
-export([mgr_start/0, mgr_start/3, mgr_init/3]).

%% Utility functions for retrieving data from the cache.
-export([get_data/1, get_mem/2, replace_data/3, replace_ttl/3,get_info/1]).

%% Functions to implement the cache data object processes.
-export([datum_start/2, datum_init/3]).

%% Cache size
-define(CSIZE, 1000).
-define(TIMEOUT, 60000).
-define(PRED1, "FoA < 2").
-define(PRED2, "FoA < 2 or Cost >= 500").
-define(PRED(X), X).


%%%--------------------------------------------
%%% Cache Manager client utility functions
%%%--------------------------------------------

get_cached_data(CacheMgr, DatumId) when is_pid(CacheMgr) ->
    case get_datum_pid(CacheMgr, DatumId) of
	{datum, Pid} -> get_data(Pid);
	Failure -> Failure
    end.

get_datum_pid(CacheMgr, DatumId) when is_pid(CacheMgr) ->
    CacheMgr ! {locate, self(), DatumId},
    receive
	{found, CacheMgr, DatumId, Pid} ->
	    {datum, Pid};
	{launched, CacheMgr, DatumId, Pid} ->
	    {datum, Pid};
	{failed, CacheMgr, DatumId, Reason} ->
	    {failed, Reason};
	{InvalidRequest, CacheMgr, DatumId, invalid_request} ->
	    {invalid_request, InvalidRequest}
%	InvalidResponse ->
%	    io:format("Here invalid ~n"),
%	    {invalid_response, InvalidResponse}
    after
	%% increase timeout from 1 secs to 5 secs
	%% due to waiting for replacement
	1000 -> {no_datum, timeout}
    end.

get_stats(CacheMgr) when is_pid(CacheMgr) ->
    io:format("Starting get stats~n"),
    CacheMgr ! {stats, self()},
    receive
	{stats, CacheMgr, AllStats} ->
	    io:format("Get Stats properly~n"),
	    AllStats;
	InvalidResponse ->
	    io:format("Get Stats ~n"),
	    InvalidResponse
    after
	1000 -> {no_cache_mgr, timeout}
    end.

%%%--------------------------------------------
%%% Datum client utility functions
%%%--------------------------------------------

get_data(DatumPid) when is_pid(DatumPid) ->
    DatumPid ! {get, self()},
    receive
	{get, DatumPid, Data} ->  {ok, Data};
	{InvalidResponse, DatumPid, Param} -> {no_data, {InvalidResponse, Param}}
	%_ -> {no_data, invalid_response}
    after
	1000 -> {no_data, timeout}
    end.

%% get state info
get_info(DatumPid) when is_pid(DatumPid) ->
    DatumPid ! {get_info, self()},
    receive
	{info, DatumPid, Info} -> {ok, Info}
    after
	1000 ->
	    {no_info, timeout}
end.


get_mem(From, DataObject) when is_pid(From), is_pid(DataObject) ->
    DataObject ! {memsize, From},
    receive
	{memsize, DataObject, SizeData} -> {ok, SizeData};
	{InvalidResponse, DataObject, Param} -> {no_size, {InvalidResponse, Param}};
	_ -> {no_size, invalid_response}
    after
	1000 -> {no_size, timeout}
    end.

%% Only the original creator of the cache object can replace the data in it.
replace_data(From, DataObject, NewData) when is_pid(From), is_pid(DataObject) ->
    DataObject ! {new_data, DataObject, NewData},
    receive
	{new_data, DataObject, OldData} -> {ok, OldData};
	{InvalidResponse, DataObject, Param} -> {no_replace, {InvalidResponse, Param}};
	_ -> {no_replace, invalid_response}
    after
	1000 -> {no_replace, timeout}
    end.

%% Only the original creator of the cache object can replace the Time To Live.
replace_ttl(From, DataObject, NewTTL) when is_pid(From), is_pid(DataObject) ->
    DataObject ! {new_ttl, DataObject, NewTTL},
    receive
	{new_ttl, DataObject, OldTTL} -> {ok, OldTTL};
	{InvalidResponse, DataObject, Param} -> {no_replace, {InvalidResponse, Param}};
	_ -> {no_replace, invalid_response}
    after
	1000 -> {no_replace, timeout}
    end.


%%%--------------------------------------------
%%% Cache Manager functions
%%%--------------------------------------------


%% start Cache Manager
mgr_start() ->
    mgr_start(double_cache, db_gen, double).


mgr_start(Name, DataModule, DataAccessor) when is_atom(Name), is_atom(DataModule), is_atom(DataAccessor) ->
    spawn(cache, mgr_init, [Name, DataModule, DataAccessor]).

mgr_init(Name, DataModule, DataAccessor) when is_atom(Name), is_atom(DataModule), is_atom(DataAccessor) ->
    EtsTab = ets:new(Name, [set,public,named_table,{write_concurrency,true},{read_concurrency,true}]),
    mgr_loop(EtsTab, DataModule, DataAccessor).


%% Cache Manager main loop
mgr_loop(EtsTab, DataModule, DataAccessor) ->
    receive

	%% A monitored datum process just went down...
%	{'DOWN', _Ref, process, _Pid, _Reason} ->
  %io:format("Notification: ~p just went down for reason ~p~n", [Pid,_Reason]),
% 	    ets:match_delete(EtsTab,{'_',_Pid}),
	{DatumId, die_pred} ->
	    ets:delete(EtsTab,DatumId),
	    mgr_loop(EtsTab, DataModule, DataAccessor);

	%% Return a process id from the cache index...
	%% From is client
	{locate, From, DatumId} when is_pid(From) ->
	    case find_or_launch_datum(DatumId, EtsTab, DataModule, DataAccessor) of
		{found, Pid} ->
		    From ! {found, self(), DatumId, Pid};
		{launched, Pid} ->
		    From ! {launched, self(), DatumId, Pid};
		Failed ->
		    From ! {failed, self(), DatumId, Failed}
		end,
	    mgr_loop(EtsTab, DataModule, DataAccessor);

	%% Invalid requests are signalled back to client...
	{InvalidRequest, From, DatumId} when is_pid(From) ->
	    From ! {InvalidRequest, self(), DatumId, invalid_request},
	    mgr_loop(EtsTab, DataModule, DataAccessor);

	%% Diagnostic queries of the cache manager...
	{stats, From} ->
	    EtsInfo = ets:info(EtsTab),
	    CacheName = proplists:get_value(name, EtsInfo),
	    DatumCount = proplists:get_value(size, EtsInfo),
	    From ! {stats, [{cache_name, CacheName},
			    {cache_size, element(2, process_info(self(), memory))},
			    {datum_count, DatumCount}]},
	    mgr_loop(EtsTab, DataModule, DataAccessor);

	%% for testing
	terminate ->
	    io:format("Cache Manager terminating ...~n"),
	    ets:delete(EtsTab);

	%% Invalid requests are signalled back to client...
	{InvalidRequest, From} when is_pid(From) ->
	    From ! {InvalidRequest, self(), invalid_request},
	    mgr_loop(EtsTab, DataModule, DataAccessor);

	%% Other shape requests cannot be handled, but cause a reload of code.
	 MalformedRequest ->
	    io:format("~p~n", MalformedRequest),
	    cache:mgr_loop(EtsTab, DataModule, DataAccessor)
    end.

find_or_launch_datum(DatumId, EtsTab, DataModule, DataAccessor) ->
    case ets:lookup(EtsTab, DatumId) of
	[{DatumId, Pid}] ->
	    {found, Pid};
	[] ->
	    %% check if the Cache is full here
	    Boolean = proplists:get_value(size, ets:info(EtsTab)) >= ?CSIZE,
	    Pid = launch_datum(DatumId, EtsTab, DataModule, DataAccessor, Boolean),
	    {launched, Pid};
	_Other ->
	    failed
end.

launch_datum(DatumId, EtsTab, DataModule, DataAccessor, false) ->
    launch_datum(DatumId, EtsTab, DataModule, DataAccessor);
launch_datum(DatumId, EtsTab, DataModule, DataAccessor, true) ->
    %io:format("The cache is full! Performing replacement ...~n"),
    {Time, _Value} = timer:tc(fun() -> reap_datum(EtsTab) end),
    io:format("~p~n",[Time/1000]),
    launch_datum(DatumId, EtsTab, DataModule, DataAccessor).

launch_datum(DatumId, EtsTab, DataModule, DataAccessor) ->
    DataToCache = DataModule:DataAccessor(DatumId),
    Pid = datum_start(DatumId, DataToCache),
    %erlang:monitor(process, Pid),
    ets:insert(EtsTab, {DatumId, Pid}),
    Pid.


%% ask all processes and eliminating based on Predicate
reap_datum(_EtsTab) ->
    %AllProcs = get_all_pids(EtsTab),
%    AllProcs = ets:tab2list(EtsTab),
%    Predicate = term_to_binary(?PRED1),
    Msg = {shutdown, self()},
    Predicate = ?PRED1,
    %send message to Pids for shutdown according to Predicate
%    [Pid || {_DatumId,Pid}<- AllProcs, (Pid ! Msg) =:= Msg],
    aerlang:send(Msg, Predicate).
%    receive
%	{_, die_pred} ->
%	    void
 %   end.

%%%--------------------------------------------
%%% Cache data object functions
%%%--------------------------------------------

-record(datum_state, {id, mgr, data, ttl, la, foa}).

datum_start(Id, DataToCache) ->
    spawn(cache, datum_init, [Id, self(), DataToCache]).

datum_init(Id, Mgr, DataToCache) ->
    LastActive = now(),
    FoA = 0,
    TTL = ?TIMEOUT,
    %io:format("Datum ~p start at ~p~n", [Id, LastActive]),
    %% Prepare the attribute environment
    %% And resgister to middleware
    Cost = random:uniform(1000),
    Env = [{'FoA', FoA}, {'Cost', Cost}],
    aerlang:register_self(Id, Env),

    datum_loop(#datum_state{id=Id, mgr=Mgr, data=DataToCache, ttl=TTL,la=LastActive, foa=FoA}).

datum_loop(#datum_state{id=Id, mgr=Mgr, data=Data, ttl=TTL, la=LastActive, foa=FoA} = State) when is_pid(Mgr) ->

    receive

	%% Cache manager indicates new data to be replaced...
	{new_data, Mgr, Replacement} ->
	    Mgr ! {new_data, self(), Data},
	    Active = now(),
	    %io:format("Last active of process ~p is ~p miliseconds~n",[Id, timer:now_diff(Active,LastActive)/1000]),
	    datum_loop(State#datum_state{data=Replacement,la=Active});


	%% Shut down from Aerlang
	{shutdown, Mgr} ->
	     	    %ets:delete(double_cache,Id),
		    Mgr ! {Id, die_pred},
		    exit(normal);

	%% refinement: cache manager asks for process age ...
	%% not reset the TTL
	{last_active, Mgr } ->
	    Active = now(),
	    IDL = round(timer:now_diff(Active,LastActive)/1000),
	    NewTTL = TTL - IDL,
	    %% io:format("age of process ~p is ~p miliseconds and NewTTL is ~p ~n",[Id,IDL,NewTTL]),
	    Mgr ! {last_active, self(), LastActive},
	    datum_loop(State#datum_state{la=Active,ttl=NewTTL});

	%% Invalid cache manager request, causes code load...
	 {InvalidMgrRequest, Mgr, _OtherData} ->
	    Mgr ! {InvalidMgrRequest, self(), invalid_creator_request},
	    cache:datum_loop(State);

	%% Request for cached data, causes FoA increases ...
	{get, From} when is_pid(From) ->
	    From ! {get, self(), Data},
	    aerlang:aupdate_attribute(Id,{'FoA',FoA+1}),
	    datum_loop(State#datum_state{la=now(),foa=FoA+1});

	%% Request for memory size used...
	{memsize, From} when is_pid(From) ->
	    Size = case is_binary(Data) of
		    true ->   {memory, size(Data)};
		    _false -> process_info(self(), memory)
	    end,
	    From ! {memsize, self(), Size},
	    datum_loop(State);

	%% Request for state information ...
	{get_info, From} when is_pid(From) ->
	    From ! {info, self(), State},
	    datum_loop(State);

	%% Invalid request, causes code load...
	{InvalidRequest, From} when is_pid(From) ->
	    From ! {InvalidRequest, self(), invalid_request},
	    cache:datum_loop(State);

	%% Any other request is ignored, but causes code load.
	_ -> cache:datum_loop(State)

    after
	%% Time To Live or TTL frees up cache data object memory (must be integer, 'infinity' or 'timeout').
	TTL ->
	    void
%	    TerminateReason = timeout,
%	    io:format("Cache object ~p owned by ~p freed because of a ~w after ~p miliseconds~n", [Id, self(), TerminateReason,TTL])
    end.


%% SEND PRIMITIVES
set_predicate(P) ->
    ?PRED(P).

supervisor(Pid) ->
    %erlang:monitor(process, Pid),
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		      {'EXIT', Pid, Why} ->
			  io:format("Cache manager died because of ~p~n",[Why]);
		      _Other  ->
			  io:format("Other reason~n")
		  end
	  end).

%% Test the concurrent cache
test() ->
    %% start the middle ware
    aerlang:start(),
    %% start cache manager
    M = mgr_start(),
    S = supervisor(M),
    io:format("Test with: CacheSize = ~p, Attribute-based Policy, Timeout ~p seconds~n",[?CSIZE,?TIMEOUT/1000]),
    io:format("Supervisor of manager ~p started ~p~n",[M,S]),
    %% start random generator
    RPid = spawn(fun() -> rand(now()) end),
    register(rand_gen, RPid),
    io:format("-> Filling cache ...~n"),
    full_cache(M,1,?CSIZE),
    %io:format("-> Sending ~p random requests ...~n ",[?REQUEST]),
    %start_requests(M,?REQUEST,true),
    full_cache(M,1,?CSIZE-1),
%    R = ?CSIZE*2,
 %   io:format("-> Sending ~p random exsting requests ...~n ",[R]),
  %  random_requests(M,R),
    io:format("-> Cache manager state ~p~n",[get_stats(M)]),

    io:format("-> More requests ~n"),
%    start_requests(M,1000001,21),
    _D = get_cached_data(M, 100000100),
   io:format("get data ~p~n",[_D]),
    timer:sleep(2000),
    io:format("-> Cache manager state ~p~n",[get_stats(M)]),
    unregister(rand_gen),
    M ! terminate.

%% start_requests/2
%% @doc Starts a given number of cached data requests.
%% -spec start_requests( M
start_requests(_,_,0) ->
    ok;
start_requests(Mgr, Base, R) when is_pid(Mgr) ->
    % Simulating access data
    _D = get_cached_data(Mgr, Base),
   io:format("get data ~p~n",[_D]),
%    timer:sleep(1000),
    start_requests(Mgr, Base+1, R-1),
    ok.

%% This function is to full the cache
full_cache(_,_,0) ->
    ok;
full_cache(Mgr, Base, R) when is_pid(Mgr) ->
    % Simulating access data
    _D = get_cached_data(Mgr, Base),
    full_cache(Mgr, Base+1, R-1),
    ok.

random_requests(_,0) ->
    ok;
random_requests(Mgr, R) when is_pid(Mgr) ->
    % Simulating access data
    RequestID = randint(?CSIZE-1),
    _D = get_cached_data(Mgr, RequestID),
%    timer:sleep(1000),
    random_requests(Mgr, R-1),
    ok.

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

get_all_pids(EtsIndex) ->
  get_all_pids(EtsIndex, ets:first(EtsIndex), []).

get_all_pids(_, '$end_of_table', Pids) -> Pids;

get_all_pids(EtsIndex, NextKey, Pids) ->
    get_all_pids(EtsIndex, ets:next(EtsIndex, NextKey), [Pid = ets:lookup_element(EtsIndex, NextKey, 2) |  Pids]).
