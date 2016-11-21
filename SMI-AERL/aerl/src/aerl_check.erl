-module(aerl_check).
-export([string/2,local_eval_send/3,local_eval_receive/2,make_fun/1,meta_eval/2]).
-include("aerl.hrl").
%% eval BExpression
string(Pred,Env) when is_list(Pred) ->
    {ok, Tokens, _} = aerl_scanner:string(Pred),
    {ok, Parsed} = aerl_parser:parse(Tokens),
    eval(Parsed,Env).

local_eval_send(Pred,Bindings,Msg) when is_list(Pred) ->
    T = get(aerl_env),
    spawn(fun() -> handle_send(Pred,Bindings,Msg,T) end).


%% THIS VERSION IS FOR EQUALITY and OR PREDICATE
handle_send(Pred,Bindings,Msg,T) ->
    Guard = aerl_decomp:make(Pred,Bindings,T),
    Env = aerl_utils:getEnv(T),
    %% RInfo = [pid,pred],
    %% Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    %% Result = [['$1','$5']],
    %%AttList = record_info(fields,aerl_store),
    %% Receivers = mnesia:dirty_select(aerl_store,[
    %% 					   {Head,
    %% 					    [Guard],
    %% 					    Result}
    %% 					  ]),
    %% lists:foreach(
    %%   fun({A,V}) ->
    %% 		 Pos = ms_util2:get_index(aerl_store,A) + 1,
    %% 		 case mnesia:dirty_index_read(aerl_store,V,Pos) of
    %% 		     [Process]  ->
    %% 			 Process#aerl_store.pid ! Msg;
    %% 		     [] ->
    %% 			 ok
    %% 		 end end,
    %% 		 Guard).
    %% if Name =/= none ->
    %% 	    Name ! Msg;
    %%    true -> ok
    %% end.
    %aerl_broker:default(Guard,Msg,Env,self()).
    aerl_broker:a_send(Guard,Msg,Env).

local_eval_receive(Pred,Bindings) when is_list(Pred) ->
    case catch aerl_env:getAtt(Pred) of
	{'EXIT',badarg} ->
	    New = aerl_scanner:scan(Pred,Bindings),
	    aerl_registry:update_pred(New),
	    aerl_env:setAtt(New,New);
	_ -> ok
    end.


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
eval({add, Left, Right} = _Exp, State) -> eval(Left, State) + eval(Right, State);
eval({sub, Left, Right} = _Exp, State) -> eval(Left, State) - eval(Right, State);
eval({mul, Left, Right} = _Exp, State) -> eval(Left, State) * eval(Right, State);
eval({'div', Left, Right} = _Exp, State) -> eval(Left, State) div eval(Right, State);
eval({var, Name} = _Exp, State) -> getAtt(Name, State);
eval(Num, _) when is_number(Num) -> Num;
eval(Atom, _) when is_atom(Atom) -> Atom;

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
getAtt(Name, State) ->
    proplists:get_value(Name,State).
