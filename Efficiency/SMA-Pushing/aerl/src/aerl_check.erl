-module(aerl_check).
-export([eval/2,local_eval_send/3,local_eval_send2/3,local_eval_receive_push/2,aerl_no_reply/1]).
-include("aerl.hrl").

%% For debug
%% print(X) ->
%%     io:format("~p~n",[X]).

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

%% THIS VERSION IS FOR GENERAL CASE + MUltiCALL receive
handle_send2(Pred,Bindings,Msg,T) ->
    LocalEval = aerl_scanner:scan(Pred,Bindings,T),
    Env = aerl_env:getEnv(T,public),
    aerl_broker:s_send(LocalEval,Msg,Env).

%% %% THIS VERSION IS FOR EQUALITY and OR PREDICATE
%% handle_send(Pred,Bindings,Msg,T) ->
%%     Guard = aerl_decomp:make(Pred,Bindings,T),
%%     Env = aerl_utils:getEnv(T),
%%     aerl_broker:a_send(Guard,Msg,Env).

aerl_no_reply(ok) -> ok;
aerl_no_reply(Pid) ->
    Pid ! aerl_no_reply.

local_eval_receive_push(Pred,Binding) ->
    T = get(aerl_env),
    Tokens = aerl_scanner:scan(Pred,Binding,T),
    {ok, Parsed} = aerl_parser:parse(Tokens),
    Parsed.


%% Evaluation part
%% Arithmic Expressions
eval({in,Left,Right} = _Exp, State) ->
    case is_list(List = eval(Right,State)) of
	true -> lists:member(eval(Left,State),lists:flatten(List));
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

%% Function
eval({func,Name,Args} = _Exp, State) ->
    List = [eval(X,State) || X <- Args],
    %%io:format("Gonna eval ~p on ~p in ~p~n",[Name,List,State]),
    R = erlang:apply(aerl_code,Name,List),
    %%io:format("Result eval ~p~n",[R]),
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
