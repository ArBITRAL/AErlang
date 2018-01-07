-module(aerl_check).
-export([eval/2,local_eval_send/3,local_eval_send2/3,local_eval_receive/2,make_fun/1,check/2]).

-include("aerl.hrl").

%% MODIFIED FOR GENERAL VERSION
local_eval_send(Pred,Bindings,Msg) when is_list(Pred) ->
    T = get(aerl_env),
    handle_send1(Pred,Bindings,Msg,T).

local_eval_send2(Pred,Bindings,Msg) when is_list(Pred) ->
    T = get(aerl_env),
    handle_send2(Pred,Bindings,Msg,T).

%% THIS VERSION IS FOR GENERAL CASE
handle_send1(Pred,Bindings,Msg,T) ->
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    Env = aerl_env:getEnv(T,public),
    aerl_broker:a_send(LocalEval,Msg,Env).

handle_send3(Pred,Bindings,Msg,T) ->
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    Env = aerl_env:getEnv(T,public),
    aerl_broker:send(LocalEval,Msg,Env).

%% THIS VERSION IS FOR GENERAL CASE + MUltiCALL receive
handle_send2(Pred,Bindings,Msg,T) ->
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    Env = aerl_env:getEnv(T,public),
    aerl_broker:s_send(LocalEval,Msg,Env).


local_eval_receive(Pred,Binding) ->
    Mode = aerl_app:get_env(aerl, mode, default),
    local_eval_receive(Pred,Binding,Mode).
local_eval_receive(Pred,Binding,pull) ->
    T = get(aerl_env),
    Tokens = aerl_scanner:scan(Pred,Binding,T),
    Fun = make_fun(Tokens),
    aerl_registry:update_pred(Tokens,self()),
    ok;
local_eval_receive(Pred,Binding,pushpull) ->
    T = get(aerl_env),
    Tokens = aerl_scanner:scan(Pred,Binding,T),
    Fun = make_fun(Tokens),
    aerl_registry:update_pred(Tokens,self()),
    ok;
local_eval_receive(Pred,Binding,_) ->
    T = get(aerl_env),
    Tokens = aerl_scanner:scan(Pred,Binding,T),
    {ok, Parsed} = aerl_parser:parse(Tokens),
    Parsed.

make_fun(Tokens) ->
    {ok, Parsed} = aerl_parser:parse(Tokens),
    F = fun(Env) -> eval(Parsed,Env) end,
    F.

check(RPred,{_,SPred,Senv} = Decoration) -> %broadcast
    Renv = aerl_env:getEnv(),
    case SPred(Renv) andalso eval(RPred,Senv) of
	true -> true;
	false ->
	    aerl_no_reply(Decoration),
	    false
    end;
check(_,{ok,_}) ->
    true;
check(RPred,{_,Senv}=Decoration) when is_list(Senv)-> %push
    case eval(RPred,Senv) of
	true -> true;
	false ->
	    aerl_no_reply(Decoration),
	    false
    end;
check(_,{_,SPred}) when is_function(SPred) -> %pull
    Renv = aerl_env:getEnv(),
    SPred(Renv).

%% Evaluation part
%% Arithmic Expressions
eval({in,Left,Right} = _Exp, State) ->
    case is_list(List = eval(Right,State)) of
	true ->
	    lists:member(eval(Left,State),lists:flatten(List));
	false -> eval(Left,State) == List
    end;
eval({'+', Left, Right} = _Exp, State) -> eval(Left, State) + eval(Right, State);
eval({'-', Left, Right} = _Exp, State) -> eval(Left, State) - eval(Right, State);
eval({'*', Left, Right} = _Exp, State) -> eval(Left, State) * eval(Right, State);
eval({'div', Left, Right} = _Exp, State) -> eval(Left, State) div eval(Right, State);

%% Comparison
eval({'==', Left, Right} = _Exp, State) ->
    eval(Left, State) == eval(Right, State);
eval({'>', Left, Right} = _Exp, State) ->
    eval(Left, State) > eval(Right, State);
eval({'<', Left, Right} = _Exp, State) ->
    eval(Left, State) < eval(Right, State);
eval({'>=', Left, Right} = _Exp, State) ->
    eval(Left, State) >= eval(Right, State);
eval({'<=', Left, Right} = _Exp, State) ->
    eval(Left, State) =< eval(Right, State);
eval({'<>', Left, Right} = _Exp, State) ->
    eval(Left, State) =/= eval(Right, State);

%% Boolean Expressions

eval(true, _) -> true;
eval(false, _) -> false;

eval({'and', Left, Right} = _Exp, State) ->
    case eval(Left, State) of
	true ->  eval(Right, State);
	false -> false
    end;
eval({'or', Left, Right} = _Exp, State) ->
    case eval(Left, State) of
	true -> true;
	false -> eval(Right, State)
    end;
eval({'not', Expression} = _Exp, State) -> true xor eval(Expression, State);

%% Function
eval({func,Name,Args} = _Exp, State) ->
    List = [eval(X,State) || X <- Args],
    R = erlang:apply(aerl_code,Name,List),
    R;
eval(Num, _) when is_number(Num) -> Num;
eval({const,Const}, _)  -> Const;
eval(List, _) when is_list(List) -> List;
eval(Name, State) when is_atom(Name) -> getAtt(Name, State).


%% Internal Functions
getAtt(Name, State) when is_list(State) ->
    proplists:get_value(Name,State);
getAtt(Name, State) when is_map(State) ->
    maps:get(Name,State).

aerl_no_reply(Data) when is_pid(element(1,Data)) ->
    element(1,Data) ! aerl_no_reply;
aerl_no_reply(_) ->
    ok.
