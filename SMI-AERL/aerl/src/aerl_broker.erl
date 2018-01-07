-module(aerl_broker).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 a_send/3,
	 a_receive/1]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mode}).

-include("../include/aerl.hrl").

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
    %%Env = aerl_env:getEnv(T),
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    gen_server:cast(?SERVER, {Pred, Msg, Env, self()}).

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
	broadcast ->
	    spawn(fun() -> broadcast(Pred,Msg,Env,Pid) end);
	pushing ->
	    spawn(fun() -> pushing(Pred,Msg,Env,Pid) end);
	pulling ->
	    spawn(fun() -> pulling(Pred,Msg,Env,Pid) end);
	default ->
	    spawn(fun() -> default(Pred,Msg,Env,Pid) end)
    end,
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

broadcast(Pred, Msg, Senv, Pid) ->
    L = mnesia:dirty_select(aerl_store,[{#aerl_store{pid = '$1', _ = '_'},[{'=/=','$1',Pid}],['$1']}]),
    Fun = aerl_check:make_fun(Pred),
    [P || P <- L, (P ! {Msg, Fun, Senv}) =:= {Msg, Fun, Senv}].
    %% lists:foreach(fun(P) ->
    %% 	P ! {Msg, Pred, Senv}
    %% end, L).

pushing(Pred, Msg, Senv, Pid) ->
    Att = att(Pred),
    Head = ms_util:make_ms(aerl_store,[pid | Att]),
    Guard = aerl_ms:make(Pred,aerl_store),
    Result = ['$1'],
    Pids = mnesia:dirty_select(aerl_store,[
    					   {Head,
    					    [Guard],
    					    Result}
    					  ]),
    [P || P <- Pids, (P ! {Msg, aerl_working_pushing, Senv}) =:= {Msg, aerl_working_pushing, Senv}].

    %% QURY THEN SELECT
    %% Head = #aerl_store{pid = '$1', env = '$2', _ = '_'},
    %% Guard = [{'=/=','$1',Pid}],
    %% Guard = [],
    %% Result = ['$$'],
    %% L = mnesia:dirty_select(aerl_store,[
    %% 					{Head,
    %% 					 Guard,
    %% 					 Result}
    %% 				       ]),
    %% Fun = aerl_check:make_fun(Pred),
    %% [P ! {Msg, aerl_working_pushing, Senv} || [P,Env] <- L, Fun(Env) =:= true].

pulling(Pred, Msg, Senv, Pid) ->
    L = mnesia:dirty_select(aerl_store,[{#aerl_store{pid = '$1', pred = '$2', _ = '_'},[{'=/=','$1',Pid}],['$$']}]),
    lists:foreach(fun([P,Rpred]) -> case rcheck(Rpred,Senv) of
	true -> P ! {Msg, Pred, aerl_working_pulling};
	false -> ok
    end end, L).

default(Guard,Msg,Senv,Pid) ->
    %%% FOR GENERAL CASE
    %% RInfo = [pid,pred],
    %% Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    %% Result = [['$1','$5']],
    %% Receivers = mnesia:dirty_select(aerl_store,[
    %% 					   {Head,
    %% 					    [Guard],
    %% 					    Result}
    %% 					  ]),
    %% [P ! Msg || [P,Rpred] <- Receivers, Rpred(Senv) =:= true].

    %% FOR EQUALITY and OR case
    lists:foreach(
      fun({A,V}) ->
    		 Pos = ms_util2:get_index(aerl_store,A) + 1,
    		 case mnesia:dirty_index_read(aerl_store,V,Pos) of
    		     [Process]  ->
    			 Process#aerl_store.pid ! Msg;
    		     [] ->
    			 ok
    		 end end,
      Guard).

a_receive(Pred) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    Rpred = evallp(Pred),
    case aerl_env:getAtt(aerl_working_mode) of
	broadcast -> receive_broadcast(Rpred,1000,StartTime);
	pushing -> receive_pushing(Rpred,infinity,StartTime);
	pulling -> receive_pulling(Rpred,infinity,StartTime)
    end.

a_receive(Pred, Timeout) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    Rpred = evallp(Pred),
    case aerl_env:getAtt(aerl_working_mode) of
	broadcast -> receive_broadcast(Rpred, Timeout, StartTime);
	pushing -> receive_pushing(Rpred, Timeout, StartTime);
	pulling -> receive_pulling(Rpred, Timeout, StartTime)
    end.

receive_broadcast(Rpred, Timeout, StartTime) ->
    Renv = aerl_env:getEnv(),
    b_receive(Rpred, Renv, Timeout, StartTime).

b_receive(Rpred, Renv, Timeout, StartTime) ->
    receive
	{aerl_working_broadcast, Spred, Msg, Senv} ->
	    case scheck(Spred, Renv) andalso rcheck(Rpred, Senv) of
		true ->
		    self() ! {aerl_message, Msg};
		false ->
		    b_receive(Rpred, Renv, time_left(StartTime,Timeout), StartTime)
	    end
    after Timeout ->
	    ok
    end.

check(Pred, aerl_working_pushing, Senv) ->
    rcheck(Pred, Senv);
check(Rpred, Spred, aerl_working_pulling) ->
    Renv = aerl_env:getEnv(),
    scheck(Spred, Renv);
check(Rpred, Spred, Senv) ->
    Renv = aerl_env:getEnv(),
    Spred(Renv)	andalso Rpred(Senv).

receive_pushing(Rpred, Timeout, StartTime) ->
    Renv = aerl_env:getEnv(),
    aerl_env:update_att(Renv),
    sh_receive(Rpred, Timeout, StartTime).

sh_receive(Rpred, Timeout, StartTime) ->
    receive
	{aerl_working_pushing, Msg, Senv} ->
	    case rcheck(Rpred, Senv) of
		true ->
		    self() ! {aerl_message, Msg};
		false ->
		    sh_receive(Rpred, time_left(StartTime,Timeout), StartTime)
	    end
    after Timeout ->
	    ok
    end.

receive_pulling(Rpred, Timeout, StartTime) ->
    aerl_env:update_pred(Rpred),
    Renv = aerl_env:getEnv(),
    ll_receive(Renv, Timeout, StartTime).

ll_receive(Renv, Timeout, StartTime) ->
    receive
	{aerl_working_pulling, Msg, Spred} ->
	    case scheck(Spred, Renv) of
		true ->
		    self() ! {aerl_message, Msg};
		false ->
		    ll_receive(Renv,time_left(StartTime,Timeout), StartTime)
	    end
    after Timeout ->
	    ok
    end.

%% Replace local reference s to the corresponding values in the local environment
evallp(P) ->
    L = string:tokens(P,"\s"),
    L1 = [case string:left(X,5) == "this." of
	      true ->
		  [_,Key] = string:tokens(X,"."),
		  lists:concat(["this." | io_lib:format("~p", [get(list_to_atom(Key))])]);
	      false -> X end || X <- L],
    string:join(L1," ").

evallp2(P) ->
    {ok, T, _} = aerl_scanner:string(P),
    T.


evallp1(P) ->
    aerl_check:make_fun(P).

scheck(Spred, Renv) ->
    %F = binary_to_term(Spred),
    Spred(Renv).

rcheck(Rpred, Senv) ->
    Rpred(Senv).
    %aerl_check:string(Rpred,Senv).

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime =  calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed*1000 of
        Time when Time =< 0 -> 0;
        Time                -> Time
    end.


att(Tokens) ->
    L = lists:usort(Tokens),
    build(L,[]).

build([],Acc) ->
    Acc;
build([{attribute,_,Att}|T], Acc) ->
    build(T,[Att|Acc]);
build([_|T],Acc) ->
    build(T,Acc).
