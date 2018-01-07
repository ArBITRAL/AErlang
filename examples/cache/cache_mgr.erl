-module(cache_mgr).
-compile(export_all).
-define(CSIZE,1000).

mgr_init(Name, DataModule, DataAccessor) when is_atom(Name), is_atom(DataModule), is_atom(DataAccessor) ->
    aerlang:register(?CSIZE*5,[]),
    EtsTab = ets:new(Name, [set,public,named_table,{write_concurrency,true},{read_concurrency,true}]),
    mgr_loop(EtsTab, DataModule, DataAccessor).


%% Cache Manager main loop
mgr_loop(EtsTab, DataModule, DataAccessor) ->
    receive
	%% A monitored datum process just went down...
	{'DOWN', _Ref, process, _Pid, _Reason} ->
  %io:format("Notification: ~p just went down for reason ~p~n", [Pid,_Reason]),
 	    ets:match_delete(EtsTab,{'_',_Pid}),
%	{DatumId, die_pred} ->
%	    ets:delete(EtsTab,DatumId),
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
    {Time, _Value} = timer:tc(rep_policy, reap_datum, [EtsTab,ca_aerl_p1]),
    io:format("~p~n",[Time/1000]),
    launch_datum(DatumId, EtsTab, DataModule, DataAccessor).

launch_datum(DatumId, EtsTab, DataModule, DataAccessor) ->
    DataToCache = DataModule:DataAccessor(DatumId),
    Pid = datum_start(DatumId, DataToCache),
    erlang:monitor(process, Pid),
    ets:insert(EtsTab, {DatumId, Pid}),
    Pid.

datum_start(Id, DataToCache) ->
    spawn(cache, datum_init, [Id, self(), DataToCache]).
