-module(aerl_env).
-export([initEnv/1,
	 newA/2,
	 setA/2,
	 setA/1,
	 getA/1,
	 getA/2,
	 restrict/1,
	 visible/1,
	 visible/2,
	 getEnv/2,
	 getEnv/1,
	 getEnv/0,
	 update_att/1,
	 update_pred/1]).

-define(ENV,?MODULE).

%% Evnvironment Handler
-spec initEnv(list()) -> ok.
initEnv(Env) ->
    T = ets:new(?ENV,[public]),
    put(?ENV,T),
    %%T = ets:new(?MODULE,[]),
    Tmp = if is_map(Env) == true -> maps:to_list(Env); true ->  Env end,
    [ets:insert(T,{A,V,public,static}) || {A,V} <- Tmp],
    ok.

-spec newA(atom(),term()) -> atom().
newA(Attribute,Value) ->
    T = get(?ENV),
    ets:insert(T,{Attribute,Value,local,static}).

%% cacheP(P,Value) ->
%%     T = get(?ENV),
%%     case ets:lookup_element(T,P,2) of
%% 	V -> V
%%     catch
%% 	_:_ -> ets:insert(T,{P,Value,local,static})
%%     end.


setA(AttList) when is_list(AttList)->
    [setA(A,V) || {A,V} <- AttList].

setA(Att,Value) ->
    T = get(?ENV),
    ets:update_element(T,Att,{2,Value}).


getA(AttributeList) when is_list(AttributeList) ->
    [getA(A) || A <- AttributeList];
getA(Attribute) ->
    T = get(?ENV),
    try ets:lookup_element(T,Attribute,2) of
	V -> V
    catch
	_:_ -> undefined
    end.

getA(T,Attribute) ->
    try ets:lookup_element(T,Attribute,2) of
	V -> V
    catch
	_:_ -> undefined
    end.


%% NEED TO IMPLEMENT ATTRIBUTE HIDDEN MECHANISM
-spec getEnv(atom()) -> list().
getEnv(public) ->
    T = get(?ENV),
    get_atts(T,public).

-spec getEnv() -> list().
getEnv() ->
    T = get(?ENV),
    get_atts(T).

getEnv(T,public) ->
    get_atts(T,public).

restrict(List) when is_list(List) ->
    [restrict(Att) || Att <- List];
restrict(Att) ->
    T = get(?ENV),
    ets:update_element(T,Att,{3,local}).


visible(Att) ->
    T = get(?ENV),
    ets:update_element(T,Att,{3,public}).

visible(Att,How) ->
    T = get(?ENV),
    ets:update_element(T,Att,[{3,public},{4,How}]).


-spec update_pred(list()) -> ok.
update_pred(Pred) ->
    aerl_registry:update_pred(Pred).

-spec update_att(map()) -> ok.
update_att(Env) ->
    aerl_registry:update_att(Env).


%% INTERNAL FUNCTIONS
get_atts(T,Visibility) ->
    Objects = ets:tab2list(T),
    [{A,V} || {A,V,Scope,_} <- Objects,Scope =:= Visibility].
get_atts(T) ->
    Objects = ets:tab2list(T),
    [{A,V} || {A,V,_,_} <- Objects].
