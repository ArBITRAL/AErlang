-module(aerl_check).
-export([string/2,local_eval_send/3,local_eval_receive/2,make_fun/1,meta_eval/2]).
-compile(export_all).
-include("aerl.hrl").
%% eval BExpression
string(Pred,Env) when is_list(Pred) ->
    {ok, Tokens, _} = aerl_scanner:string(Pred),
    {ok, Parsed} = aerl_parser:parse(Tokens),
    eval(Parsed,Env).


%% MODIFIED FOR GENERAL VERSION
local_eval_send(Pred,Bindings,Msg) when is_list(Pred) ->
    T = get(aerl_env),
    %%spawn(fun() -> handle_send1(Pred,Bindings,Msg,T) end).
    handle_send1(Pred,Bindings,Msg,T).

local_eval_send2(Pred,Bindings,Msg) when is_list(Pred) ->
    T = get(aerl_env),
    %%spawn(fun() -> handle_send2(Pred,Bindings,Msg,T) end).
    handle_send2(Pred,Bindings,Msg,T).

%% THIS VERSION IS FOR GENERAL CASE
handle_send1(Pred,Bindings,Msg,T) ->
    %%io:format("Before transformation Pred ~p, Bindings ~p ~n",[Pred,Bindings]),
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    %%Guard = aerl_guard:make(Pred,Bindings,T),
    %%io:format("Guard after transformation ~p ~n",[Guard]),
    Env = aerl_utils:getEnv(T),
    %%[aerl_broker:a_send(X,Msg,Env) || X <- Guard].
    aerl_broker:a_send(LocalEval,Msg,Env).

%% THIS VERSION IS FOR GENERAL CASE + MUltiCALL receive
-spec handle_send2(term(),term(),term(),term()) -> number().
handle_send2(Pred,Bindings,Msg,T) ->
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    %Guard = aerl_guard:make(LocalEval),
    Env = aerl_utils:getEnv(T),
    %%[aerl_broker:a_send(X,Msg,Env) || X <- Guard].
    aerl_broker:s_send(LocalEval,Msg,Env).


%% THIS VERSION IS FOR EQUALITY and OR PREDICATE
handle_send(Pred,Bindings,Msg,T) ->
    Guard = aerl_decomp:make(Pred,Bindings,T),
    Env = aerl_utils:getEnv(T),
    %%[aerl_broker:a_send(X,Msg,Env) || X <- Guard].
    aerl_broker:a_send(Guard,Msg,Env).


local_eval_receive(Pred,Bindings) when is_list(Pred) ->
    %% case catch aerl_env:getAtt(Pred) of
    %% 	Pred ->
    %% 	    ok;
    %% 	_ ->
	    T = get(aerl_env),
	    Pid = self().
	    %%spawn(fun() -> handle_receive(Pred,Bindings,T,Pid) end).
%%    end.

handle_receive(Pred,Binding,T,Pid) ->
    LocalEval = aerl_scanner:scan(Pred,Binding,T),
    aerl_registry:update_pred(LocalEval, Pid).
    %%aerl_utils:setAtt(T,Pred,Pred).


make_fun(Tokens) ->
    {ok, Parsed} = aerl_parser:parse(Tokens),
    F = fun(Env) ->
     		eval(Parsed,Env) end,
    F.

meta_eval({Fname,[Arg]}=F,State) ->
    A = getAtt(Arg,State),
    Fname(A);
meta_eval(F,State) ->
    F(State).


func(Pred) when is_list(Pred) ->
    {ok, Tokens, _} = aerl_scanner:string(Pred),
    io:format("FUNC After scanner ~p~n",[Tokens]),
    {ok, Parsed} = aerl_parser:parse(Tokens),
    io:format("FUNC After parsed ~p~n",[Parsed]),
    F = fun(Env) ->
		Parsed end.


%% Arithmic Expressions
eval({in,Left,Right} = _Exp, State) ->
    case is_list(List = eval(Right,State)) of
	true -> lists:member(eval(Left,State),lists:flatten(List));
	false -> eval(Left,State) == List
    end;
eval({add, Left, Right} = _Exp, State) -> eval(Left, State) + eval(Right, State);
eval({sub, Left, Right} = _Exp, State) -> eval(Left, State) - eval(Right, State);
eval({mul, Left, Right} = _Exp, State) -> eval(Left, State) * eval(Right, State);
eval({'div', Left, Right} = _Exp, State) -> eval(Left, State) div eval(Right, State);
eval({attribute, Name} = _Exp, State) -> getAtt(Name, State);
eval(Num, _) when is_number(Num) -> Num;
eval(Atom, _) when is_atom(Atom) -> Atom;
eval(List, _) when is_list(List) -> List;

%% Comparison
eval({eq, Left, Right} = _Exp, State) ->
    eval(Left, State) == eval(Right, State);
eval({ge, Left, Right} = _Exp, State) ->
    eval(Left, State) > eval(Right, State);
eval({le, Left, Right} = _Exp, State) ->
    eval(Left, State) < eval(Right, State);
eval({geq, Left, Right} = _Exp, State) ->
    eval(Left, State) >= eval(Right, State);
eval({leq, Left, Right} = _Exp, State) ->
    eval(Left, State) =< eval(Right, State);
eval({diff, Left, Right} = _Exp, State) ->
    eval(Left, State) =/= eval(Right, State);

%% Boolean Expressions
eval({'and', Left, Right} = _Exp, State) ->
    case eval(Left, State) of
	true ->
	    eval(Right, State);
	false -> false
    end;
eval({'or', Left, Right} = _Exp, State) ->
    case eval(Left, State) of
	true -> true;
	false -> eval(Right, State)
    end;
eval({'not', Expression} = _Exp, State) -> true xor eval(Expression, State);
eval(true, _) -> true;
eval(false, _) -> false.

%% Internal Functions
getAtt(Name, State) when is_list(State) ->
    proplists:get_value(Name,State);
getAtt(Name, State) when is_map(State) ->
    maps:get(Name,State).
