-module(aerl).

-export([start/0,
	 stop/0,
	 register/1,
	 register/2,
	 unregister/0,
	 unregister/1]).

-export([a_send/2,
	 a_receive/1]).

-export([getEnv/0,
	 setAtt/2,
	 getAtt/1,
	 setAtts/1,
	 getAtts/1
	]).

start() ->
    ok = mnesia:start(),
    ok = wpool:start(),
    ok = application:start(aerl).

stop() ->
    mnesia:stop(),
    wpool:stop(),
    application:stop(aerl).

register(Env) ->
    aerl_registry:register(Env).

register(Pid, Env) ->
    aerl_registry:register(Pid, Env).

unregister() ->
    aerl_registry:unregister().

unregister(Pid) ->
    aerl_registry:unregister(Pid).

a_send(Pred,Msg) ->
    aerl_broker:a_send(Pred, Msg).

a_receive(Pred) ->
    aerl_broker:a_receive(Pred).

getEnv() ->
    aerl_env:getEnv().

getAtt(Name) ->
    aerl_env:getAtt(Name).

getAtts(List) ->
    aerl_env:getAtts(List).

setAtt(Name, Value) ->
    aerl_env:setAtt(Name, Value).

setAtts(TupleList) ->
    aerl_env:setAtts(TupleList).
