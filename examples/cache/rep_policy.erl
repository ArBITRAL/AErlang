-module(rep_policy).
-compile({parse_transform,aerl_trans}).

-compile(export_all).

-define(PRED1, "FoA < 2").
-define(PRED2, "FoA < 2 or Cost > 500").


%% ask all processes and eliminating based on Predicate
%% 3 lines of code for each policy P1, P2
reap_datum(_EtsTab, ca_aerl_p1) ->
    Msg = {shutdown, self()},
    Predicate = ?PRED1,
    to(Predicate) ! Msg;

reap_datum(_EtsTab, ca_aerl_p2) ->
    Msg = {shutdown, self()},
    Predicate = ?PRED2,
    to(Predicate) ! Msg;

%% LRU replacement BEGIN
reap_datum(DatumIndex, c_erl_lru) ->
    AllProcs = ets:tab2list(DatumIndex),
    Msg = {last_active, self()},
    % send message to Pids for asking their age
    Pids = [Pid || {_DatumId, Pid} <- AllProcs, (Pid ! Msg) =:= Msg],
    NumPids = length(Pids),
    reap_oldest(NumPids, 200, none).

reap_oldest(0, _Timeout, none) ->
    ok;
reap_oldest(0, _Timeout, {OldestPid, _WhenActive}) ->
    Msg = {shutdown, self()},
    OldestPid ! Msg,
    ok;
reap_oldest(NumPids, Timeout, OldestPidPair) ->
    receive
	{last_active, Pid, WhenLastActive} ->
	    NewOldest = choose_oldest(OldestPidPair, {Pid, WhenLastActive}),
	    reap_oldest(NumPids-1, Timeout, NewOldest)
    after
	Timeout ->
	    reap_oldest(0, Timeout, OldestPidPair)
    end.

choose_oldest(none, PidPair) ->
    PidPair;
choose_oldest({_Pid1, When1} = Pair1, {_Pid2, When2} = Pair2) ->
    case When1 < When2 of
	true -> Pair1;
	false -> Pair2
    end.
%% LRU Replacement END
