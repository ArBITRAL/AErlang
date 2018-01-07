-module(big).

-export([run/3]).

run([N|_], _, _) ->
    %ok = aerl:start(),
    %% spawn N processes
    Procs = spawn_procs(N),

    %% trigger them to start ping
    send_procs(Procs, {procs, Procs, self()}),

    RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
    %% wait for all to be done by receving all message of form {done, PId}
    receive_msgs(RMsgs),
    %% terminate all process
    lists:foreach(fun (P) -> P ! die end, Procs), ok.
    %ok = aerl:stop().

%% when created, each process wait for a trigger from Parent
%% include list of all procs, and Parent PiD
pinger([], [], true) ->
	receive
		{procs, Procs, ReportTo} -> pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
	receive
		{ping, From} ->
			From ! {pong, self()},
			pinger([],[],false);
		die ->
			ok
	end;
pinger([], [], ReportTo) ->
	ReportTo ! {done, self()},
	pinger([],[],false);
pinger([],[Po|Pos] = Pongers, ReportTo) ->
	receive
	{ping, From} ->
		From ! {pong, self()},
		pinger([], Pongers, ReportTo);
	{pong, Po} ->
		pinger([], Pos, ReportTo)
	end;

% first goes here
pinger([Pi|Pis], Pongers, ReportTo) ->
    % if there any ping in mailbox, if any, reply with a pong
    receive
	{ping, From} ->
	    From ! {pong, self()}
    after 0 -> ok
    end,
    % send a ping, add that pid to list of pongers
    Pi ! {ping, self()},
    pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =< 0 ->
	[];
spawn_procs(N) ->
	[spawn_link(fun () -> pinger([], [], true) end) | spawn_procs(N-1)].

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
