-module(aerlang).
-behaviour(gen_server).
-export([start/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).


%% interface functions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

register(Key,Pid, Meta) -> gen_server:call(?MODULE, {new, Key, Pid, Meta}).
register_self(Key, Meta) -> gen_server:call(?MODULE, {new, Key, Meta}).
unregister(Key) -> gen_server:call(?MODULE, {remove, Key}).
update_attribute(Key, NewMeta) -> gen_server:call(?MODULE, {update,
Key, NewMeta}).
aupdate_attribute(Key, NewMeta) -> gen_server:cast(?MODULE, {update,
Key, NewMeta}).

find_pid(Key) ->
   case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_}] -> Pid
    end.

find_key(Pid) ->
    Result =  ets:match(?MODULE,{'$1',Pid,'_','_'}),
    case Result of
	[] -> undefined;
	[[Key]] -> Key;
	_ -> Result
    end.

%gen_server:call(?MODULE,{find,Key}).

%% Objects of Meta structure assuming
%% Key, Pid, [{K1,V1},{K2,V2},...]

%% Let the aerlang determine which processes are targets for sending
%% by evaluating the predicate
asend(Msg, Pred) ->
%    send_all_env(?MODULE,Msg,Pred).
    gen_server:cast(?MODULE, {send, Msg, Pred}).

send(Msg,Pred) ->
    gen_server:call(?MODULE, {send, Msg, Pred}).

get_env_by_key(Key) ->
    ets:lookup_element(?MODULE, Key, 4).

get_env_by_pid(Pid) ->
    Result =  ets:match(?MODULE,{'_',Pid,'_','$1'}),
    case Result of
	[] -> undefined;
	[[Env]] -> Env;
	_ -> Result
    end.



init([]) -> {ok, ets:new(?MODULE,[set, private, named_table, {write_concurrency, true}, {read_concurrency, true}])}.

handle_call({new, Key, Pid, Meta}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
		[]  ->
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Key, Pid, Ref, Meta});
		[_] -> {already_registered}
	    end,
    {reply, Reply, Tab};
handle_call({new, Key, Meta}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
		[]  ->
		    {Pid, _Tag} = _From,
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Key, Pid, Ref, Meta});
		[_] -> {already_registered}
	    end,
    {reply, Reply, Tab};
handle_call({remove, Key}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Key) of
		[]  -> {undefined};
		[{_,_,Ref,_}] ->
		    erlang:demonitor(Ref),
		    ets:delete(Tab, Key)
	    end,
    {reply, Reply, Tab};
handle_call({update,Key,NewMeta}, _From, Tab) ->
    Reply = case ets:lookup_element(Tab, Key, 4) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
	    end,
    {reply, Reply, Tab};
handle_call({find,Key}, _From, Tab) ->
    Reply =  case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_}] -> Pid
    end,
    {reply, Reply, Tab};
handle_call({send,Msg,Pred}, _From, Tab) ->
    Reply = send_all_env(?MODULE,Msg,Pred),
    {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
    ets:delete(Tab),
    {stop, normal, stopped, Tab}.

handle_cast({send,Msg,Pred}, State) ->
    send_all_env(?MODULE,Msg,Pred),
    {noreply, State};
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
    ets:match_delete(?MODULE,{'_',Pid,'_','_'}),
    {noreply, State};
handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% helper functions
send_all_env(EtsIndex,Msg, Pred) ->
  send_all_env(EtsIndex, ets:first(EtsIndex), Msg, Pred).

send_all_env(_, '$end_of_table', _, _) -> done;
send_all_env(EtsIndex, NextKey, Msg, Pred) ->
    [{_,Pid,_,Meta}] = ets:lookup(EtsIndex, NextKey),
    case eval(Pred,Meta) of
	true ->
	    %io:format("Gonna send ~p to ~p~n",[Msg,Pid]),
	    Pid ! Msg;
	false -> void
    end,
    send_all_env(EtsIndex, ets:next(EtsIndex, NextKey), Msg, Pred).


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
    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
    {value,Result,_} = erl_eval:exprs(Parsed,Env),
    Result.
