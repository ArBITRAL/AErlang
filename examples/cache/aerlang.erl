-module(aerlang).
-behaviour(gen_server).
-export([start/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).

%% An entry has five parts: Key, Pid, Ref, Env, Predicate.!

%% interface functions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

register_with_pid(Key,Pid, Meta) -> gen_server:call(?MODULE, {new, Key, Pid, Meta}).
register(Key, Meta) -> gen_server:call(?MODULE, {new, Key, Meta}).

unregister(Key) -> gen_server:call(?MODULE, {remove, Key}).

%% Asynchronous update attributes
update(Key, NewMeta) -> gen_server:cast(?MODULE, {update,
Key, NewMeta}).

%% Synchronous update attributes
supdate(Key, NewMeta) -> gen_server:call(?MODULE, {update_att,
Key, NewMeta}).

%% Asynchornous update predicate by Pid
set_predicate(Pid, Pred) ->
    gen_server:call(?MODULE, {update_pred, Pid, Pred}).


find_pid(Key) ->
   case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_,_}] -> Pid
    end.

find_key(Pid) ->
    Result =  ets:match(?MODULE,{'$1',Pid,'_','_','_'}),
    case Result of
	[] -> undefined;
	[[Key]] -> Key;
	_ -> Result
    end.

%gen_server:call(?MODULE,{find,Key}).

%% Objects of Meta structure assuming
%% Key, Pid, [{K1,V1},{K2,V2},...]

%% Asynchronous (default)
send(Msg, Pred) ->
    gen_server:cast(?MODULE, {async_send, Msg, Pred}).

%% Synchronous
ssend(Msg,Pred) ->
    gen_server:call(?MODULE, {sync_send, Msg, Pred}).

send_with_threshold(Msg,Pred,T1) ->
    gen_server:call(?MODULE, {send_with_threshold, Msg, Pred, T1}).

get_env_by_key(Key) ->
%    io:format("LOOK UP~p",[Key]),
    ets:lookup_element(?MODULE, Key, 4).

get_env_by_pid(Pid) ->
    Result =  ets:match(?MODULE,{'_',Pid,'_','$1','_'}),
    case Result of
	[] -> undefined;
	[[Env]] -> Env;
	_ -> Result
    end.



init([]) ->
    State = ets:new(?MODULE,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, State}.

handle_call({new, Key, Pid, Meta}, _From, Tab) ->
    %% new registration always have receiving Predicate 'true'
    Predicate = "tt",
    Reply = case ets:lookup(Tab, Key) of
		[]  ->
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Key, Pid, Ref, Meta,Predicate});
		    %io:format("Register infor ~p,~p,~p,~p~n",[Key,Pid,Ref,Meta]);
		[_] -> {already_registered}
	    end,
    {reply, Reply, Tab};

handle_call({new, Key, Meta}, _From, Tab) ->
    Predicate = "tt",
    Reply = case ets:lookup(Tab, Key) of
		[]  ->
		    {Pid, _Tag} = _From,
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Key, Pid, Ref, Meta,Predicate});
		[_] -> {already_registered}
	    end,
    {reply, Reply, Tab};
handle_call({remove, Key}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
		[]  -> {undefined};
		[{_,_,Ref,_,_}] ->
		    erlang:demonitor(Ref),
		    ets:delete(Tab, Key)
	    end,
    {reply, Reply, Tab};
handle_call({update_att,Key,NewMeta}, _From, Tab) ->
    Reply = case ets:lookup_element(Tab, Key, 4) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
	    end,
    {reply, Reply, Tab};
handle_call({update_pred,Pid,Pred}, _From, Tab) ->

    Reply = case ets:match(?MODULE,{'$1',Pid,'_','_','_'}) of
	[] -> undefined;
	[[Key]] -> ets:update_element(Tab, Key, {5,Pred})
	    end,
%    io:format("Updated Pred ~p~n",[Pred]),
    {reply, Reply, Tab};

handle_call({find,Key}, _From, Tab) ->
    Reply =  case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_,_}] -> Pid
    end,
    {reply, Reply, Tab};
handle_call({sync_send,Msg,Pred}, From, Tab) ->
    {Sender,_} = From,
%    io:format("~p has ~p~n",[Sender,ets:match(?MODULE, {'_',Sender,'_','$1','_'})]),
    [[Env_sender]] = ets:match(?MODULE, {'_',Sender,'_','$1','_'}),
    Reply = send_all_env(?MODULE,Msg,Pred,Env_sender),
    {reply, Reply, Tab};

handle_call({send_with_threshold,Msg,Pred,T1}, _From, Tab) ->
    Reply = send_with_threshold(?MODULE,Msg,Pred,T1),
    {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
    ets:delete(Tab),
    {stop, normal, stopped, Tab}.


%% Todo
handle_cast({async_send,Msg,Pred}, Tab) ->
%    {Sender,_} = From,
%    [[Env_sender]] = ets:match(?MODULE, {'_',Sender,'_','$1','_'}),
%    Reply = send_all_env(?MODULE,Msg,Pred,Env_sender),
    {noreply, Tab};
%handle_cast({asyn_send,Msg,Pred}, State) ->
%    [[Env_sender]] = ets:match(?MODULE, {'_',Sender,'_','$1','_'}),
%    send_all_env(?MODULE,Msg,Pred,Env_sender),
%    {noreply, State};
handle_cast({update,Key,NewMeta}, Tab) ->
    case ets:lookup_element(Tab, Key, 4) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
	    end,
    {noreply, Tab};
handle_cast(_,State) -> {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(?MODULE,{'_',Pid,'_','_','_'}),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% helper functions
send_all_env(EtsIndex,Msg, Pred, Env) ->
  send_all_env(EtsIndex, ets:first(EtsIndex), Msg, Pred, Env).

send_all_env(_, '$end_of_table', _, _,_) -> done;
send_all_env(EtsIndex, NextKey, Msg, Ps, Envs) ->
    [{_,Pid,_,Envr,Pr}] = ets:lookup(EtsIndex, NextKey),
    case eval(Ps,Envr) andalso eval(Pr,Envs) of
	true ->
%	    io:format("Sender: ~p ~p, Receiver ~p ~p~n",[Ps,Envs,Pr,Envr]),
%	    io:format("Gonna send ~p to ~p~n",[Msg,Pid]),
	    Pid ! Msg;
	false ->
	    %io:format("Sender: ~p ~p, Receiver ~p ~p~n",[Ps,Envs,Pr,Envr]),
	    void
    end,
    send_all_env(EtsIndex, ets:next(EtsIndex, NextKey), Msg, Ps, Envs).

%% helper functions
send_with_threshold(EtsIndex,Msg, Pred,T1) ->
  send_with_threshold(EtsIndex, ets:first(EtsIndex), Msg, Pred, T1).

send_with_threshold(_, '$end_of_table', _, _, _) -> done;
send_with_threshold(EtsIndex, NextKey, Msg, Pred, T1) ->
    if T1 > 0 ->
	    [{_,Pid,_,Meta,_}] = ets:lookup(EtsIndex, NextKey),
	    T2 = case eval(Pred,Meta) of
		true ->
			 Pid ! Msg,
			 T1 -1;
		false ->
			 T1
	    end,
	    send_with_threshold(EtsIndex, ets:next(EtsIndex, NextKey), Msg, Pred,T2);
       true -> void
    end.


%% Helper functions
%% Convert the Predicate string to Erlang Expression
pred(Pi) ->
    case Pi of
	"tt" -> "true.";
	"ff" -> "false.";
	_ ->
	    T1 = re:replace(Pi, " and ", " andalso ",[global,{return,list}]),
	    T2 = re:replace(T1, " or ", " orelse ",[global,{return,list}]),
	    T3 = re:replace(T2, " <= ", " =< ",[global,{return,list}]),
	    T4 = re:replace(T3, " tt ", " true ",[global,{return,list}]),
	    T5 = re:replace(T4, " ff ", " false ",[global,{return,list}]),
	    T6 = re:replace(T5, " = ", " == ",[global,{return,list}]),
	    T6 ++ "."
end.

%% Evaluate a string containing an Erlang Experssion
eval(Pi,Meta) ->
%    io:format("Satisfaction ~p ~p ~n",[Pi, Meta]),
    case Pi of
	"tt" -> true;
	"ff" -> false;
	_Other ->
	    case Meta of
		[] -> false;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
		    {value,Result,_} = erl_eval:exprs(Parsed,Env),
		    Result
	    end
    end.
