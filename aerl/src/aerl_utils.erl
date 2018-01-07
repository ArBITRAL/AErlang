-module(aerl_utils).
-include("aerl.hrl").

-export([getAtt/2,
	 getEnv/1,
	 getIndex/2
	 ]).

-compile(export_all).

%% Evnvironment Handler Support

-spec getAtt(atom(),atom()) -> term().
getAtt(T,Attribute) ->
    aerl_env:getA(T,Attribute).

-spec getEnv(atom()) -> list().
getEnv(T) ->
    aerl_env:getEnv(T).

-spec setAtt(term(), atom(),term()) -> atom().
setAtt(T,Attribute,Value) ->
    ets:insert(T,{Attribute,Value}).

getIndex(E, aerl_store) ->
    Fields = record_info(fields, aerl_store),
    pos(E,Fields);
getIndex(E, aerl_pub) ->
    Fields = record_info(fields, aerl_pub),
    pos(E,Fields);
getIndex(E, aerl_sub) ->
    Fields = record_info(fields, aerl_sub),
    pos(E,Fields).

pos(_, []) ->
    -1;
pos(E,[E|_]) ->
    1;
pos(E,[_|T]) ->
    1+pos(E,T).
