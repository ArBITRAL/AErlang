%% This initial implementation is for broadcast mode
-module(aerl).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).

%% An entry has five parts: Pid, Ref, Env, Predicate.!

%% interface functions
start(Mode) when Mode==broadcast;Mode==push;Mode==pull ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Mode, []).

stop() -> gen_server:call(?MODULE, stop).

register(Meta) ->
%        io:format("register command Pid ~p~n",[self()]),
        gen_server:call(?MODULE, {new, Meta}).
unregister(Key) -> gen_server:call(?MODULE, {remove, Key}).

%% Asynchronous update attributes
update(Key, NewMeta) ->
    gen_server:cast(?MODULE, {update, Key, NewMeta}).

%% Synchronous update attributes
supdate(Key, NewMeta) ->
    gen_server:call(?MODULE, {update_att, Key, NewMeta}).

%% Update predicate by Pid
update_pred(Pred) ->
    case mode() of
	pull -> gen_server:call(?MODULE, {update_pred, Pred});
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
send(Pred,Msg,Env) ->
    Pid = self(),
    V = [],
    %io:format("Going aerl send"),
    case is_tuple(Pred) of
	true -> {Bind,_Ps} = Pred,
		Ps = evallp(_Ps,Env),
		%io:format("~p is gonna send with ~p, ~p, ~p~n",[Pid,Msg,Ps,Bind]),
		gen_server:cast(?MODULE, {async_send, Ps, Msg, Bind, Env, Pid});
	false -> % io:format("~p is gonna send with ~p, ~p~n",[Pid,Pred,V]),
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
    Predicate = "ff",
    Reply = case ets:lookup(Tab, Pid) of
		[]  ->
		    Ref = erlang:monitor(process, Pid),
%		    io:format("Register infor ~p,~p,~p,~p~n",[Pid,Ref,Meta,Predicate]),
		    ets:insert(Tab, {Pid, Ref, Meta, Predicate});
		[_] -> {already_registered}
	    end,
    {reply, Reply, State};

handle_call({update_att,Key,NewMeta}, _From, {Tab,_}=State) ->
    Reply = case ets:lookup_element(Tab, Key, 4) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
	    end,
    {reply, Reply, State};

handle_call({update_pred, Pred}, From, {Tab,_}=State) ->
    spawn(fun() -> update_handler(Pred,From,Tab) end),
    {noreply, State};

handle_call({find,Key}, _From, {Tab,_}=State) ->
    Reply =  case ets:lookup(?MODULE,Key) of
	 [] -> undefined;
	 [{Key,Pid,_,_,_}] -> Pid
    end,
    {reply, Reply, State};

handle_call(stop, _From, {Tab,_}=State) ->
    ets:delete(Tab),
    {stop, normal, stopped, State}.

handle_cast({async_send,Pred,Msg,V,Env,Pid}, {Tab,Mode}=State) ->
    case Mode of
	broadcast -> spawn(fun() -> broadcast(?MODULE,Msg,Pred,V,Env,Pid) end);
	push ->
	    % spawn(fun() -> pushing(?MODULE,Msg,Pred,V,Pid) end);
	    spawn(fun() -> pushing(?MODULE,Msg,Pred,V,Env,Pid) end);
	pull ->
	    spawn(fun() -> pulling(?MODULE,Msg,Pred,V,Pid) end)
    end,
    {noreply, State};

handle_cast({update,Key,NewMeta}, {Tab,_}=State) ->
    case ets:lookup_element(Tab, Key, 4) of
		[]  -> not_registered;
		L ->
		    {Id1,V1} = NewMeta,
		    NewEnv = lists:keyreplace(Id1,1,L,{Id1,V1}),
		    ets:update_element(Tab, Key, {4,NewEnv})
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
update_handler(Pred, {Pid,_}=From, Tab) ->
    ets:update_element(Tab,Pid,{4,Pred}),
    io:format("Updated Pred ~p~n",[Pred]),
    gen_server:reply(From, ok).


%% broadcast mode
broadcast(EtsIndex,Msg, Pred, Bind, Env, Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','_','_'},[{'=/=','$1',Pid}],['$1']}]),
%    [P || P <- L, (P ! {Pred,Msg,Env}) =:= {Pred,Msg,Env}].
    [P || P <- L, (P ! {Pred,Msg,Bind,Env}) =:= {Pred,Msg,Bind,Env}].

%% pushing mode
pushing(EtsIndex,Msg, Pred, Bind,Envs, Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','$2','_'},[{'=/=','$1',Pid}],['$$']}]),
    lists:foreach(fun([P,Env]) -> case check_spred(Pred,Env) of
	true -> P ! {"tt",Msg,Envs};
	false -> ok
    end end, L).

%% pulling mode
pulling(EtsIndex,Msg,Pred,Env,Pid) ->
    L = ets:select(EtsIndex,[{{'$1','_','_','$2'},[{'=/=','$1',Pid}],['$$']}]),
    lists:foreach(fun([P,Pr]) -> case check_rpred(Pr,Env) of
	true -> P ! {Pred,Msg,Env};
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
%	    io:format("What is ~p ~n",[T7]),
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

%% Evaluate a string containing an Erlang Experssion
-spec check_spred(Pi::string(), Er::map() | list()) -> boolean().
check_spred(Pi,Er) ->
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
    		    %% io:format("Parsed ~p~n",[Scanned]),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
    		    %% io:format("Parsed ~p~n",[Parsed]),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} -> Result
		    catch
			error:_ -> false
		    end
	    end
    end.



-spec check_spred2(Pi::string(), Er::map() | list(), Bind::list()) -> boolean().
check_spred2(Pi,Er,Bind) ->
%    io:format("Check Ps ~p for ~p with Er ~p and Bind ~p~n",[Pi,self(),Er,Bind]),
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
	    %io:format("META1++ is ~p~n",[Meta1++Bind]),
	    %io:format("META is ~p~n",[Meta]),
	    case Meta of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pi)),
    		    %io:format("New Predicate ~p~n",[pred(Pi)]),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
    		    %% io:format("Parsed ~p~n",[Parsed]),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} -> %io:format("Result Ps for ~p is ~p~n",[self(),Result]),
					    Result
		    catch
			error:_ -> %io:format("Result false Ps ~n"),
			    false
		    end
	    end
    end.


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
    		    %% io:format("Parsed ~p~n",[Scanned]),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Meta),
    		    %% io:format("Parsed ~p~n",[Parsed]),
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
%	    io:format("Check Receiving Predicate ~p for ~p with Bind ~p~n",[pred(Pr),self(),Bind1]),
	    case Bind1 of
		[] -> true;
		_ ->
		    {ok,Scanned,_} = erl_scan:string(pred(Pr)),
		    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
		    Env = lists:foldl(fun({K,V}, Bindings) ->
			      erl_eval:add_binding(K, V, Bindings) end,
		      erl_eval:new_bindings(), Bind1),
    		     %io:format("Parsed vs Env ~p ~p~n",[Parsed,Env]),
		    try erl_eval:exprs(Parsed,Env) of
			{value,Result,_} -> %io:format("Result Pr for ~p is ~p~n",[self(),Result]),
					    Result
		    catch
			error:_ -> %io:format("Result Pr false ~n"),
				   false
		    end
	    end
    end.


% the evaluation of predicate P on the local environment Env
-spec evallp(P::string() | tuple(),E::[X]) -> P1::string() | tuple() when X::tuple().
evallp({B,P},E) ->
    L = re:split(P,"\s",[{return,list}]),
    L1 = [case string:left(X,5) == "this." of
	      true ->
		  Key = re:replace(X,"this.","",[{return,list}]),
		  lists:flatten(io_lib:format("~p", [maps:get(list_to_atom(Key),E)]));
	      false -> X end || X <-L],
    {B,string:join(L1," ")};
evallp(P,E) ->
    L = re:split(P,"\s",[{return,list}]),
    L1 = [case string:left(X,5) == "this." of
	      true ->
		  Key = re:replace(X,"this.","",[{return,list}]),
		  lists:flatten(io_lib:format("~p", [maps:get(list_to_atom(Key),E)]));
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


%% Pr is receiving predicate
%% Msg is a tuple of pair containing variable bindings transformed from the receving message in user code
%% aerl_broadcast_receive(Pr,Env) ->
%%     receive
%% 	{_Ps, Msg, _Bind, _Envs} ->
%%     		       case check_spred2(_Ps, Env, _Bind) andalso
%% 			   check_rpred2(Pr,,_Envs)
%%     			   of
%% 			   true ->
%% 			       self() ! Msg;
%% 			   false -> aerl_broadcast_receive(Pr,Env)
%%     		       end
%%     end.

flush() ->
        receive
                _ -> flush()
        after
                0 -> ok
        end.
