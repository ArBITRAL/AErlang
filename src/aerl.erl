-module(aerl).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).

%% An entry has four parts: Pid, Ref, Env, Predicate.!

%% Predicate = {BindingList,String}

%% interface functions
start(Mode) when Mode==broadcast;Mode==pushing;Mode==pulling ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Mode, []).

stop() -> gen_server:call(?MODULE, stop).

register(Environment) ->
    initEnv(Environment),
    Mode = mode(),
    put(mode,Mode),
    gen_server:call(?MODULE, {new, Environment}).

register(Pid, Environment) ->
    initEnv(Environment),
    Mode = mode(),
    put(mode,Mode),
    gen_server:call(?MODULE, {new, Pid, Environment}).

unregister(Key) -> gen_server:call(?MODULE, {remove, Key}).

%% Asynchronous update attributes
update_att(Key, NewMeta) ->
    gen_server:cast(?MODULE, {update_att, Key, NewMeta}).

%% Synchronous update attributes
supdate_att(Key, NewMeta) ->
    gen_server:call(?MODULE, {supdate_att, Key, NewMeta}).

%% Update predicate by Pid
update_pred(Pred) ->
    case get(mode) of
	pulling -> gen_server:call(?MODULE, {update_pred, Pred});
	_ -> ok
    end.

mode() ->
    gen_server:call(?MODULE,mode).

%% addtional functionallity
%register_with_pid(Key,Pid, Meta) -> gen_server:call(?MODULE, {new, Key, Pid, Meta}).

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

%% Asynchronous (default)
send(Pred,Msg) ->
    Env = get(),
    Pid = self(),
    V = [],
    case is_tuple(Pred) of
	true -> {Bind,_Ps} = Pred,
		Ps = evallp(_Ps,Env),
		gen_server:cast(?MODULE, {async_send, Ps, Msg, Bind, Env, Pid});
	false ->
	    Ps = evallp(Pred,Env),
	    gen_server:cast(?MODULE, {async_send, Ps, Msg, V, Env, Pid})
    end.

%% Synchronous
ssend(Msg,Pred) ->
    gen_server:call(?MODULE, {sync_send, Msg, Pred}).


init(Mode) ->
    Tab = ets:new(?MODULE,[set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    io:format("AErlang started with ~p~n",[Mode]),
    {ok, {Tab,Mode}}.

handle_call(mode, _From, {_,Mode}=State) ->
    {reply,Mode,State};

handle_call({new, Meta}, {Pid,_Tag}, {Tab,_}=State) ->
    Predicate = {[],"ff"},
    Reply = case ets:lookup(Tab, Pid) of
		[]  ->
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Pid, Ref, maps:to_list(Meta), Predicate});
		[_] -> {already_registered}
	    end,
    {reply, Reply, State};

handle_call({new, Pid, Meta}, _From, {Tab,_}=State) ->
    Predicate = {[],"ff"},
    Reply = case ets:lookup(Tab, Pid) of
		[]  ->
		    Ref = erlang:monitor(process, Pid),
		    ets:insert(Tab, {Pid, Ref, maps:to_list(Meta), Predicate});
		[_] -> {already_registered}
	    end,
    {reply, Reply, State};

%% DEPREDICATED
handle_call({supdate_att,Key,NewMeta}, _From, {Tab,_}=State) ->
    Reply = case ets:lookup_element(Tab, Key, 2) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
	    end,
    {reply, Reply, State};

handle_call({update_pred, Pred}, From, {Tab,_}=State) ->
    spawn(fun() -> update_pred_handler(Pred,From,Tab) end),
    {noreply, State};

handle_call({find,Key}, _From, {_Tab,_}=State) ->
    Reply =  case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_,_}] -> Pid
    end,
    {reply, Reply, State};

handle_call(stop, _From, {Tab,_}=State) ->
    ets:delete(Tab),
    {stop, normal, stopped, State}.

handle_cast({async_send,Pred,Msg,V,Env,Pid}, {_,Mode}=State) ->
    case Mode of
	broadcast -> spawn(fun() -> broadcast(?MODULE,Msg,Pred,V,Env,Pid) end);
	pushing ->
	    spawn(fun() -> push(?MODULE,Msg,Pred,V,Env,Pid) end);
	pulling ->
	    spawn(fun() -> pull(?MODULE,Msg,Pred,V,Env,Pid) end)
    end,
    {noreply, State};

%% update attribute values at the column 3 of the table
handle_cast({update_att,Key,NewMeta}, {Tab,_}=State) ->
    case ets:lookup_element(Tab, Key, 3) of
	[]  -> not_registered;
	L ->
	    TL1 = lists:keysort(1,NewMeta),
	    TL2 = lists:keysort(1,L),
	    NewEnv = lists:keymerge(1,TL1,TL2),
	    ets:update_element(Tab, Key, {3,NewEnv})
    end,
    {noreply, State};

handle_cast(_,State) -> {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(?MODULE,{'_',Pid,'_','_','_'}),
    {noreply, State};

%% other callbacks
handle_info(_, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% update predicate (at position 4) in the table
update_pred_handler(Pred, {Pid,_}=From, Tab) ->
    ets:update_element(Tab,Pid,{4,Pred}),
    gen_server:reply(From, ok).


%% broadcast mode
broadcast(EtsIndex,Msg, Pred, Bind, Envs, Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','_','_'},[{'=/=','$1',Pid}],['$1']}]),
    [P || P <- L, (P ! {Pred,Msg,Bind,Envs}) =:= {Pred,Msg,Bind,Envs}].

%% pushing mode
push(EtsIndex,Msg, Pred, Bind, Envs, Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','$2','_'},[{'=/=','$1',Pid}],['$$']}]),
    lists:foreach(fun([P,Env]) -> case check_spred(Pred,Bind,Env) of
	true -> P ! {"tt",Msg,[],Envs};
	false -> ok
    end end, L).

%% pulling mode
pull(EtsIndex,Msg,Pred,BindS,Envs,Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','_','$2'},[{'=/=','$1',Pid}],['$$']}]),
    lists:foreach(fun([P,{BindR,Pr}]) -> case check_rpred2(Pr,BindR,Envs) of
	true -> P ! {Pred,Msg,BindS,[]};
	false -> ok
    end end, L).


%% Helper functions
%% Convert the Predicate string to Erlang Expression
pred(Pi) ->
    case Pi of
	"tt" -> "true.";
	"ff" -> "false.";
	_ ->
	    T1 = re:replace(capF(Pi, " and "), " and ", " andalso ",[global,{return,list}]),
	    T2 = re:replace(capF(T1," or "), " or ", " orelse ",[global,{return,list}]),
	    T3 = re:replace(capfirst(T2), " <= ", " =< ",[global,{return,list}]),
	    T4 = re:replace(capfirst(T3), " tt ", " true ",[global,{return,list}]),
	    T5 = re:replace(capfirst(T4), " ff ", " false ",[global,{return,list}]),
	    T6 = re:replace(capfirst(T5), " = ", " == ",[global,{return,list}]),
	    [T7] = capF(T6,"not "),
	    T7 ++ "."
    end.

%% Change the first letter to captialize
capF(Pi,Word) ->
    L = re:split(Pi, Word, [{return,list}]),
    case length(L) > 1 of
	true ->
	   % string:join(L,Word);
	   lists:foldl(fun(X, Out) -> capfirst(X) ++ Word ++ Out end, "", lists:droplast(L)) ++ capfirst(lists:last(L));
	false -> capfirst(L)
end.

capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.

%% SERVER: check sending predicate inside receiving Environment
-spec check_spred(Pi::string(), Bind::list(), Er::map() | list()) -> boolean().
check_spred(Pi,Bind,Er) ->
    case Pi of
	"tt" -> true;
	"ff" -> false;
	_Other ->
	    Meta1 = if is_map(Er) -> maps:to_list(Er);
		       true -> Er
		    end,
	    Meta = lists:map(
		     fun({X,Y}) -> {list_to_atom(capfirst(atom_to_list(X))),Y} end,
		     Meta1++Bind),
	    case Meta of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} ->
					    Result
		    catch
			error:_ ->
			    false
		    end
	    end
    end.


% CLIENT: check sending predicate in receiving environment,
-spec check_spred2(Pi::string(), Bind::list()) -> boolean().
check_spred2(Pi,Bind) ->
    Er = getEnv(),
    case Pi of
	"tt" -> true;
	"ff" -> false;
	_Other ->
	    Meta1 = if is_map(Er) -> maps:to_list(Er);
		       true -> Er
		    end,
	    Meta = lists:map(
		     fun({X,Y}) -> {list_to_atom(capfirst(atom_to_list(X))),Y} end,
		     Meta1++Bind),
	    case Meta of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} ->
					    Result
		    catch
			error:_ ->
			    false
		    end
	    end
    end.

%% OUT DATED
-spec check_rpred(Pi::string(), Er::map() | list()) -> boolean().
check_rpred(Pi,Er) ->
    case Pi of
	"tt" -> true;
	"ff" -> false;
	_Other ->
	    Meta1 = if is_map(Er) -> maps:to_list(Er);
		       true -> Er
		    end,
	    Meta = lists:map(
		     fun({X,Y}) -> {list_to_atom(capfirst(atom_to_list(X))),Y} end,
		     Meta1),
	    case Meta of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} -> Result
		    catch
			error:_ -> false
		    end
	    end
    end.


pred2(Pi) ->
    {ok,T,_} = erl_scan:string(Pi),
    lists:foldl(fun(X,New) ->
			case X of
			    {var,_,Some} -> [Some] ++ New ;
			    _ -> New
			end
		end,[], T).



-spec check_rpred2(P::string(), Bind::list(), Envs::map() | list()) -> boolean().
check_rpred2(P,Bind,Envs) ->
    case P of
	"tt" -> true;
	"ff" -> false;
	_Other ->
	    if is_tuple(P) ->
		    {B,Pr} = P;
	       true -> Pr = P, B = []
	    end,
	    Meta1 = if is_map(Envs) -> maps:to_list(Envs);
		       true -> Envs
		    end,
	    Meta = lists:map(
		     fun({X,Y}) -> {list_to_atom(capfirst(atom_to_list(X))),Y} end,
		     Meta1),
	    Bind1 = lists:merge(Meta,Bind) ++ B,
	    case Bind1 of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pr)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Bind1),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} ->
					    Result
		    catch
			error:_ -> false
		    end
	    end
    end.


% the evaluation of predicate P on the local environment Env
-spec evallp(P::string() | tuple(),E::[X]) -> P1::string() | tuple() when X::tuple().
evallp({B,P},_E) ->
    L = re:split(P,"\s",[{return,list}]),
    L1 = [case string:left(X,5) == "this." of
	      true ->
		  Key = re:replace(X,"this.","",[{return,list}]),
		  lists:flatten(io_lib:format("~p", [get(list_to_atom(Key))]));
	      false -> X end || X <-L],
    {B,string:join(L1," ")};
evallp(P,_E) ->
    L = re:split(P,"\s",[{return,list}]),
    L1 = [case string:left(X,5) == "this." of
	      true ->
		  Key = re:replace(X,"this.","",[{return,list}]),
		  lists:flatten(io_lib:format("~p", [get(list_to_atom(Key))]));
	      false -> X end || X <-L],
    string:join(L1," ").

% the evaluation of expressions V on the local environment Env
-spec evalle(V::tuple(),E::map()) -> V1::list(X) when X::tuple().
evalle(V,E) ->
    [case string:left(X,5) == "this." of
	     true ->
		 Key = list_to_atom(re:replace(X,"this.","",[{return,list}])),
		 {Key,maps:get(Key,E)};
	     false -> {X,X} end || X <- tuple_to_list(V)].

flush() ->
        receive
                _ -> flush()
        after
                0 -> ok
        end.

%%%%% Functions to handle Evnvironment
-spec setAtt(atom(),term()) -> atom().
setAtt(Attribute,Value) ->
    put(Attribute,Value),
    case get(mode) of
	pushing -> update_att(self(),[{Attribute,Value}]);
	_ -> ok
    end.

-spec setAtts(list()) -> atom().
setAtts(List) ->
    lists:foreach(fun({A,V}) -> put(A,V) end,List),
    case get(mode) of
	pushing -> update_att(self(),List);
	_ -> ok
    end.

-spec getAtt(atom()) -> term().
getAtt(Attribute) ->
    get(Attribute).

-spec getAtts(list()) -> list().
getAtts(AttributeList) ->
    lists:foldr(fun(A,Result) -> [get(A) | Result] end, [], AttributeList).

-spec getEnv() -> list().
getEnv() ->
    get().

-spec initEnv(list()) -> ok.
initEnv(Env) ->
    case is_map(Env) of
	true -> Tmp = maps:to_list(Env);
	false -> Tmp = Env
    end,
    lists:foreach(fun({A,V}) -> setAtt(A,V) end,Tmp).
