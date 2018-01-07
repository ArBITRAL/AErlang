-module(aerl_broker).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 a_send/3,
	 send/3,
	 s_send/3,
	 a_receive/2]).
-export([build_rguard/1]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{mode}).

-include("aerl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Mode) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Mode) ->
    %% wpool:start_pool(
    %%   ?MODULE, [
    %%             {workers, 100},
    %%             {worker, {?SERVER, [Mode]}}
    %%            ]
    %% ).
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mode], []).

a_send(Pred, Msg, Env) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    %%gen_server:cast(?SERVER, {Pred, Msg, Env}).
    gen_server:cast(?SERVER, {Pred, Msg, Env, self()}).

send(Pred, Msg, Env) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    %%gen_server:cast(?SERVER, {Pred, Msg, Env}).
    gen_server:cast(?SERVER, {normal, Pred, Msg, Env, self()}).

s_send(Pred, Msg, Env) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    gen_server:call(?SERVER, {Pred, Msg, Env}).

a_receive(Pred, Pid) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    gen_server:cast(?SERVER, {Pred, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Mode]) ->
    {ok, #state{mode=Mode}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({Pred,Msg,Env}, From, State) ->
    case State#state.mode of
	pushpull ->
	    pushpull(Pred,Msg,Env,From);
	push ->
	    push(Pred,Msg,Env,From);
	broadcast ->
	    bcast(Pred,Msg,Env,From);
	pull ->
	    pull(Pred,Msg,Env,From)
    end,
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({Pred,Msg,Env,Pid}, State) ->
    case State#state.mode of
	pushpull ->
	        spawn(fun() -> pushpull(Pred,Msg,Env,Pid) end);
	push ->
	        spawn(fun() -> push(Pred,Msg,Env,Pid) end);
	broadcast ->
	    %% test
	    spawn(fun() -> bcast(Pred,Msg,Env,Pid) end);
	pull ->
	        spawn(fun() -> pull(Pred,Msg,Env,Pid) end)
    end,
    {noreply, State};
handle_cast({normal,Pred,Msg,Env,Pid}, State) ->
    case State#state.mode of
	pushpull ->
	        spawn(fun() -> pushpull(Pred,Msg,Env,Pid) end);
	push ->
	        spawn(fun() -> push(Pred,Msg,Env,Pid) end);
	broadcast ->
	    spawn(fun() -> push(Pred,Msg,Env,Pid) end);
	pull ->
	        spawn(fun() -> pull(Pred,Msg,Env,Pid) end)
    end,
    {noreply, State};
handle_cast({Pred,Pid}, State) ->
    spawn(fun() -> handle_receive(Pred,Pid) end),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% For handling the asynchronous send
%% Pred is tokens returned from scanner
%% normal send

%% default(Pred,Msg,Senv,opt2) ->
%%     %%FOR EQUALITY and AND/OR case, FAST!
%%     DNF = aerl_dnf:make(Pred),
%%     Recv = lists:map(
%%      fun(Clause) -> %% For each condition ~ sending predicate
%% 	      Rec = ms_util:make_rec(aerl_store,Clause),
%% 	      Procs = mnesia:dirty_match_object(Rec), %% match a list of processes
%% 	      lists:foldl(fun(Process,Sum) ->
%% 	      			  Process#aerl_store.pid ! {Msg,{Senv}},
%% 	      			  Sum + 1
%% 	      		  end, 0, Procs)
%%      end,
%% 	     DNF).
%%     %count_msg(lists:sum(Recv)+1).

%% default2(Pred,Msg,Senv,{Pid,_} = From,opt) ->
%%     %%% FOR GENERAL CASE WITH VERBOSE attributes as collumn names
%%     [Att,Guard] = aerl_guard:make(Pred),
%%     Head = ms_util:make_ms(aerl_store,[pid|Att]),
%%     Result = [['$1']],
%%     %%io:format(" ~p send ~p to ~p ~n",[Senv,Msg,Pred]),
%%     %% print(Head),
%%     %% print(Guard),
%%     %% print(Result),
%%     Recv = mnesia:dirty_select(aerl_store,[
%%     					   {Head,
%%     					    [Guard],
%%     					    Result}
%%     			 		  ]),
%%     N = length(Recv),
%%     gen_server:reply(From,N),
%%     [P ! {Msg,{Pid,Senv}} || [P] <- Recv];
%% default2(Pred,Msg,Senv,{Pid,_}=From,opt2) ->
%%     %%FOR EQUALITY and AND/OR case, FAST!
%%     DNF = aerl_dnf:make(Pred),
%%     Recv = lists:map(
%%      fun(Clause) ->
%% 	      Rec = ms_util:make_rec(aerl_store,Clause),
%% 	      Procs = mnesia:dirty_match_object(Rec),
%% 	     %%io:format("~p send to ~p~n",[Senv,Procs]),
%% 	      lists:foldl(fun(Process,Sum) ->
%% 	      			  Process#aerl_store.pid ! {Msg,Pid,Senv},
%% 	      			  Sum + 1
%% 	      		  end, 0, Procs)
%%       end,
%%       DNF),
%%     %count_msg(lists:sum(Recv)+1),
%%     gen_server:reply(From,lists:sum(Recv)).

bcast(Pred,Msg,Senv,Pid) when is_pid(Pid) ->
   % Start = os:timestamp(),
    [Att,Guard] = aerl_guard:bguard(Pred,Pid),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    L = mnesia:dirty_select(aerl_store,[{Head,[Guard],['$1']}]),
    %L = mnesia:dirty_select(aerl_store,[{#aerl_store{pid = '$1', _ = '_'},[{'=/=','$1',Pid}],['$1']}]),
    Fun = aerl_check:make_fun(Pred),
    Recv = [P ! {Msg, {ok,Fun,Senv}} || P <- L],
%    Now = timer:now_diff(os:timestamp(), Start)/1000,
    %% ets:insert(broker,{Pid,Now}),
    %Size = length(Recv)*erlang_term:byte_size({Msg,{Pid,Fun,Senv}}),
    %count_req(Size),
    count_msg(length(Recv)),
    %% count_service(length(L2)),
    %ets:insert(req,{PId,length(L2)}),
    ok;
bcast(Pred,Msg,Senv,{Pid,_}=From) ->
    [Att,Guard] = aerl_guard:bguard(Pred, Pid),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store, RInfo ++ Att),
    Result = ['$1'],
    %% Return = mnesia:dirty_select(aerl_store,[{Head,[Guard],Result}]),
    %L = mnesia:dirty_select(aerl_store,[{#aerl_store{pid = '$1', _ = '_'},[{'=/=','$1',Pid}],['$1']}]),
    L = mnesia:dirty_select(aerl_store, [{Head, [Guard], Result}]),
    %%io:format("BCAST SELECTION ~p ~n",[length(L)]),
    Fun = aerl_check:make_fun(Pred),
    Recv = [P ! {Msg, {Pid, Fun, Senv}} || P <- L],
    gen_server:reply(From,length(Recv)),
    count_msg(length(Recv)),
    %Size = length(Recv)*erlang_term:byte_size({Msg,{Pid,Fun,Senv}}),
    %count_req(Size),
    ok.


push(Pred,Msg,Senv,Pid) when is_pid(Pid) ->
   % Start = os:timestamp(),
    [Att,Guard] = aerl_guard:make(Pred),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    Result = ['$1'],
    Recv = mnesia:dirty_select(aerl_store,[{Head,[Guard],Result}]),
    %Fun = fun([P]) -> P ! {Msg, {ok, Senv}} end,
    %L2 = pmap(Fun, Recv -- [[Pid]]),
    L2 = [P ! {Msg, {ok, Senv}} || P <- Recv, Pid =/= P],
    count_msg(length(L2)),
    %Size = length(L2)*erlang_term:byte_size({Msg,{ok,Senv}}),
    %count_req(Size),
    %Now = timer:now_diff(os:timestamp(), Start)/1000,
    %% ets:insert(broker,{Pid,Now}),
    %% count_req(1),
    %% count_service(length(L2)),
    ok;
push(Pred, Msg, Senv, {Pid, _} = From) ->
    [Att, Guard] = aerl_guard:make(Pred),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store, RInfo ++ Att),
    Result = ['$1'],
    Recv = mnesia:dirty_select(aerl_store, [{Head, [Guard], Result}]),
%    Fun = fun([P],Recept) -> P ! {Msg, {Recept, Senv}} end,
 %   L2 = pmap(Fun, Recv -- [[Pid]]),
    L2 = [P ! {Msg, {Pid, Senv}} || P <- Recv, Pid =/= P],
    gen_server:reply(From, length(L2)),
    %[Pid ! M1 || M1 <- L2],
    %%gen_server:reply(From, L2),
    count_msg(length(L2)),
    %Size = length(L2)*erlang_term:byte_size({Msg,{Pid,Senv}}),
    %count_req(Size),
    ok.


pull(Pred, Msg, Env, Pid) when is_pid(Pid) ->
   % Start = os:timestamp(),
    Fun = aerl_check:make_fun(Pred),
    L = mnesia:dirty_select(aerl_sub, [{#aerl_sub{pid = '$1', pred = '$2', _ = '_'}, [{'=/=', '$1', Pid}], ['$$']}]),
    L2 = [P ! {Msg, {Fun, ok}} || [P, RPred] <- L, RPred =/= undefined andalso RPred(Env) == true],
    %Now = timer:now_diff(os:timestamp(), Start)/1000,
    %% ets:insert(broker,{Pid,Now}),
    %% count_req(1),
    %% count_service(length(L2)),
    %% case Recv of
    %% 	[] ->
	    %% Now = now_to_micro_seconds(os:timestamp()),
	    %% NewEnv = Env ++ [{pid,Pid},{env,Env},{pred,Fun},{msg,Msg},{time,Now}],
	    %% Rec1 = ms_util:make_rec(aerl_pub,NewEnv),
	    %% mnesia:dirty_write(Rec1),
    %% 	_ -> ok
    %% end,
    ok;
pull(Pred, Msg, Env, {Pid, _} = From) ->
    gen_server:reply(From, ok),
    Fun = aerl_check:make_fun(Pred),
    L = mnesia:dirty_select(aerl_sub,[{#aerl_sub{pid = '$1', pred = '$2', _ = '_'},[{'=/=','$1',Pid}],['$$']}]),
    Recv = [P ! {Msg,{Fun,Pid}} || [P,RPred] <- L, RPred =/= undefined andalso RPred(Env) == true],
    case Recv of
	[] ->
	    Now = now_to_micro_seconds(os:timestamp()),
	    NewEnv = Env ++ [{pid,Pid},{env,Env},{pred,Fun},{msg,Msg},{time,Now}],
	    Rec1 = ms_util:make_rec(aerl_pub,NewEnv),
	    %% store message into table for pulling later
	    mnesia:dirty_write(Rec1);
	_ -> ok
    end,
    ok.


pushpull(Pred,Msg,Env,Pid) when is_pid(Pid) ->
    %Start = os:timestamp(),
    Fun = aerl_check:make_fun(Pred),
    [Att,Guard] = aerl_guard:make(Pred),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    Result = [['$1']],
    Recv = mnesia:dirty_select(aerl_store,[{Head,[Guard],Result}]),
    RGuard = build_rguard(Recv),
    case RGuard of
	[] -> pull(Pred,Msg,Env,Pid);
	_ ->
	    L = mnesia:dirty_select(aerl_sub,[{#aerl_sub{pid = '$1', pred = '$2', _ = '_'},[RGuard],[['$1','$2']]}]),
	    L2 = [P ! {Msg,{ok}} || [P,RPred] <- L, RPred =/= undefined andalso RPred(Env) == true, Pid =/= P]
	    %% Now = timer:now_diff(os:timestamp(), Start)/1000,
	    %% ets:insert(broker,{Pid,Now}),
	    %% count_service(length(L2)),
	    %% count_req(1)
    end,
    ok;
pushpull(Pred,Msg,Env,From) ->
    gen_server:reply(From,ok),
    Fun = aerl_check:make_fun(Pred),
    [Att,Guard] = aerl_guard:make(Pred),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    Result = [['$1']],
    Recv = mnesia:dirty_select(aerl_store,[{Head,[Guard],Result}]),
    lists:foreach(fun([P]) ->
			 [RPred] = mnesia:dirty_select(aerl_sub,[{#aerl_sub{pid = '$1', pred = '$2', _ = '_'},[{'==','$1',P}],['$2']}]),
			  case RPred =/= undefined andalso RPred(Env) of
			      true -> P ! {Msg, {ok}};
			      false -> ok
			  end
		  end, Recv),
    ok.

handle_receive(RPred,Pid) ->
    Now = now_to_micro_seconds(os:timestamp()),
    Fun = aerl_check:make_fun(RPred),
    [Att,Guard] = aerl_guard:make(RPred),
    TimeGuard = {'and',{'=<',{'-',Now,'$5'},10000},Guard},
    RInfo = [pid,pred,msg,time],
    Head = ms_util:make_ms(aerl_pub,RInfo ++ Att),
%    io:format("My table ~p~n",[ets:tab2list(aerl_pub)]),
    Result = [['$1','$3','$4']],
 %   io:format("Retrive from table with  ~p, ~p , ~p ~n", [Head,TimeGuard,Result]),
    mnesia:subscribe({table, aerl_pub, simple}),
    Recv = mnesia:dirty_select(aerl_pub,[{Head,[TimeGuard],Result}]),
%    count_req(1),
    Re = pull_msg(Recv,Pid),
  %  io:format("Re ~p~n",[Recv]),
    %Re = [Pid ! {Msg,{SPred}} || [P,SPred,Msg] <- Recv, Pid =/= P],
 %   io:format("Filter ~p~n",[Re]),
    checksubscribe(Re,Fun,Pid),
    %%L = mnesia:dirty_select(aerl_pub,[{#aerl_pub{pid = '$1', env = '$2', pred = '$3', msg='$4'},[{'=/=','$1',Pid}],['$$']}]),
    ok.

pull_msg([],Pid) -> [];
pull_msg([[Pid,SPred,Msg]|Recv],Pid) -> pull_msg(Recv,Pid);
pull_msg([[_P,SPred,Msg]|Recv],Pid) ->
    Pid ! {Msg,{SPred}}.

checksubscribe([],RPred,Pid) ->
    receive
	{_,{_,Rec,_}} when element(2,Rec) =/= Pid ->
	    Env = element(3,Rec),
	    case RPred(Env) of
		true ->
		    Pid ! {element(5,Rec),{element(4,Rec)}};
		false ->
		    checksubscribe([],RPred,Pid)
	    end
    end;
checksubscribe(_,_,_) -> ok.


build_rguard([]) -> [];
build_rguard([[Pid]]) ->
    {'==','$1',Pid};
build_rguard([[P1]|Recv]) ->
    {'or',{'==','$1',P1},build_rguard(Recv)}.


seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.

count_service(0) -> ok;
count_service(X) when X > 0 ->
    ets:update_counter(service,num,X);
count_service(X) ->
    io:format("What is this ~p ~n",[X]).


count_req(0) -> ok;
count_req(X) when X > 0 ->
    ets:update_counter(request,num,X);
count_req(X) ->
    io:format("What is this ~p ~n",[X]).

count_msg(0) -> ok;
count_msg(X) when X > 0 ->
    ets:update_counter(message,num,X);
count_msg(X) ->
    io:format("What is this ~p ~n",[X]).

pmap(F, L) ->
    S = self(),
    Ref = make_ref(),
    lists:foreach(fun(I) ->
		    spawn(fun() -> do_f1(S, Ref, F, I) end)
	    end, L),
    %% gather the results
    gather1(length(L), Ref, []).

do_f1(Parent, Ref, F, I) ->
    C = self(),
    io:format("Send ~p to ~p ~n",[C,I]),
    catch F(I,C),
    Ret =
	receive
	    Msg ->
		Msg
	end,
    Parent ! {Ref, Ret}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
    receive
	{Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
    end.
