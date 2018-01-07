-module(aerl_utils).

-export([getAtt/2,
	 getEnv/1
	 ]).


%% Evnvironment Handler Support

-spec getAtt(atom(),atom()) -> term().
getAtt(T,Attribute) ->
    ets:lookup_element(T,Attribute,2).

-spec getEnv(atom()) -> list().
getEnv(T) ->
    ets:tab2list(T).
