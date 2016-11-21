-module(aerl_utils).

-export([getAtt/2,
	 getEnv/1
	 ]).

-compile(export_all).

%% Evnvironment Handler Support

-spec getAtt(atom(),atom()) -> term().
getAtt(T,Attribute) ->
    ets:lookup_element(T,Attribute,2).

-spec getEnv(atom()) -> list().
getEnv(T) ->
    ets:tab2list(T).

-spec setAtt(term(), atom(),term()) -> atom().
setAtt(T,Attribute,Value) ->
    ets:insert(T,{Attribute,Value}).
