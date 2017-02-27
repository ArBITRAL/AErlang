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
	 getEnv/1,
	 newAtt/2,
	 setAtt/1,
	 setAtt/2,
	 getAtt/1,
	 initEnv/1,
	 restrict/1,
	 visible/1,
	 visible/2
	]).


start() ->
    ok = mnesia:start(),
    %%ok = application:start(tinymq),
    ok = application:start(aerl).

stop() ->
    mnesia:stop(),
    %%application:stop(tinymq),
    %%wpool:stop(),
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


initEnv(Env) ->
    aerl_env:initEnv(Env).

visible(Att) ->
    aerl_env:visible(Att,static).

visible(Att,How) ->
    aerl_env:visible(Att,How).

restrict(Att) ->
    aerl_env:restrict(Att).

getEnv() ->
    aerl_env:getEnv().

getEnv(Visibility) ->
    aerl_env:getEnv(Visibility).

newAtt(Name, Value) ->
    aerl_env:newA(Name, Value).

setAtt(Name, Value) ->
    aerl_env:setA(Name, Value).

setAtt(List) ->
    aerl_env:setA(List).

getAtt(Name) ->
    aerl_env:getA(Name).



%% getEnv() ->
%%     aerl_env:getEnv().

%% getAtt(Name) ->
%%     aerl_env:getAtt(Name).

%% getAtts(List) ->
%%     aerl_env:getAtts(List).

%% setAtt(Name, Value) ->
%%     aerl_env:setAtt(Name, Value).

%% setAtts(TupleList) ->
%%     aerl_env:setAtts(TupleList).

%% last_act() ->
%%     aerl_broker:last_act().
