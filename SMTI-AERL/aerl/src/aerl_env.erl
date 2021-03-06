-module(aerl_env).
-export([initEnv/1,
	 setAtt/2,
	 setAtts/1,
	 getAtt/1,
	 getAtts/1,
	 getEnv/0,
	 update_att/1,
	 update_pred/1]).


%% Evnvironment Handler
-spec initEnv(list()) -> ok.
initEnv(Env) ->
    T = ets:new(?MODULE,[public,{read_concurrency, true},{write_concurrency, true}]),
    put(?MODULE,T),
    Tmp = if is_map(Env) == true -> maps:to_list(Env); true ->  Env end,
    [ets:insert(T,D) || D <- Tmp].

-spec setAtt(atom(),term()) -> atom().
setAtt(Attribute,Value) ->
    T = get(?MODULE),
    ets:insert(T,{Attribute,Value}).

-spec setAtts(list()) -> atom().
setAtts(List) ->
    T = get(?MODULE),
    [ets:insert(T,{A,V}) || {A,V} <- List].

-spec getAtt(atom()) -> term().
getAtt(Attribute) ->
    T = get(?MODULE),
    ets:lookup_element(T,Attribute,2).

-spec getAtts(list()) -> list().
getAtts(AttributeList) ->
    T = get(?MODULE),
    [ets:lookup_element(T,A,2) || A <- AttributeList].

-spec getEnv() -> list().
getEnv() ->
    T = get(?MODULE),
    ets:tab2list(T).


%% NEED TO IMPLEMENT ATTRIBUTE HIDDEN MECHANISM
-spec getEnv(atom()) -> list().
getEnv(public) ->
    T = get(?MODULE).

-spec update_pred(list()) -> ok.
update_pred(Pred) ->
    aerl_registry:update_pred(Pred).

-spec update_att(map()) -> ok.
update_att(Env) ->
    aerl_registry:update_att(Env).
