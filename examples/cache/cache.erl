-module(cache).

-export([test/0]).
%% Utility functions for clients contacting the cache manager.
-export([get_cached_data/2, get_stats/1]).
-export([get_datum_pid/2]).

%% Cache Manager functions.
-export([mgr_start/0, mgr_start/3]).

%% Utility functions for retrieving data from the cache.
-export([get_data/1, get_mem/2, replace_data/3, replace_ttl/3,get_info/1]).

%% Functions to implement the cache data object processes.
-export([datum_init/3]).

%% Cache size
-define(CSIZE, 1000).
-define(TIMEOUT, 15000).

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
	    {invalid_request, InvalidRequest};
	InvalidResponse ->
	    {invalid_response, InvalidResponse}
    after
	1000 -> {no_datum, timeout}
    end.

get_stats(CacheMgr) when is_pid(CacheMgr) ->
    CacheMgr ! {stats, self()},
    receive
	{stats, CacheMgr, AllStats} ->
	    AllStats;
	InvalidResponse ->
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
	{InvalidResponse, DatumPid, Param} -> {no_data, {InvalidResponse, Param}};
	_ -> {no_data, invalid_response}
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
    spawn(cache_mgr, mgr_init, [Name, DataModule, DataAccessor]).


%%%--------------------------------------------
%%% Cache data object functions
%%%--------------------------------------------

-record(datum_state, {id, mgr, data, ttl, la, foa}).

datum_init(Id, Mgr, DataToCache) ->
    LastActive = os:timestamp(),
    FoA = 0,
    TTL = 30000, %% 30 seconds
    %io:format("Datum ~p start at ~p~n", [Id, LastActive]),
    %% Prepare the attribute environment
    %% And resgister to middleware
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    Cost = random:uniform(1000),
    Env = [{'FoA', FoA}, {'Cost', Cost}],
    aerlang:register(Id, Env),
    datum_loop(#datum_state{id=Id, mgr=Mgr, data=DataToCache, ttl=TTL,la=LastActive, foa=FoA}).

datum_loop(#datum_state{id=Id, mgr=Mgr, data=Data, ttl=TTL, la=LastActive, foa=FoA} = State) when is_pid(Mgr) ->
    receive
	%% Cache manager indicates new data to be replaced...
	{new_data, Mgr, Replacement} ->
	    Mgr ! {new_data, self(), Data},
	    Active = os:timestamp(),
	    datum_loop(State#datum_state{data=Replacement,la=Active});


	%% Shut down from Aerlang
	{shutdown, Mgr} ->
		    exit(normal);

	%% refinement: cache manager asks for process age ...
	%% not reset the TTL
	{last_active, Mgr } ->
	    Active = os:timestamp(),
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
	    aerlang:update(Id,{'FoA',FoA+1}),
	    datum_loop(State#datum_state{la=os:timestamp(),foa=FoA+1});

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
    end.


supervisor(Pid) ->
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
    RPid = spawn(fun() -> rand() end),

    register(rand_gen, RPid),
    io:format("-> Filling cache ...~n"),
    full_cache(M,1,?CSIZE),
    R = (?CSIZE) * 2,
    io:format("-> Sending ~p random requests ...~n ",[R]),
    random_requests(M,R),


    %io:format("-> Sending ~p exsting requests ...~n ",[R]),
    %full_cache(M,1,R),
    io:format("-> Cache manager state ~p~n",[get_stats(M)]),

    io:format("-> More request ~n"),
%    start_requests(M,1000001,21),
    _D = get_cached_data(M, 100000100),
   io:format("get data ~p~n",[_D]),
    timer:sleep(2000),
    io:format("-> Cache manager state ~p~n",[get_stats(M)]),
    unregister(rand_gen),
    aerlang:stop(),
    M ! terminate.

%% start_requests/2
%% @doc Starts a given number of cached data requests.
%% -spec start_requests( M
start_requests(_,_,0) ->
    ok;
start_requests(Mgr, Base, R) when is_pid(Mgr) ->
    % Simulating access data
    _D = get_cached_data(Mgr, Base),
%   io:format("get data ~p~n",[_D]),
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

rand() ->
    receive
        {From, N} when is_pid(From) ->
	    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	    random:seed(A,B,C),
            Int = random:uniform(N),
            From ! {randgen, Int},
            rand();
        _Other ->
            rand()
    end.

randint(N) ->
    rand_gen ! {self(), N},
    receive
	{randgen, Int} ->
	    Int
    after
	1000 -> {error, timeout}
    end.
