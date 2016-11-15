-module(aerl_ms).
-export([make/2]).

make(Pred,Env) when is_list(Pred) ->
    {ok, Parsed_Tree} = aerl_parser:parse(Pred),
    ms(Parsed_Tree, Env).

%% Arithmic Expressions
ms({add, Left, Right} = _Exp, Record) -> {'+', ms(Left, Record), ms(Right, Record)};
ms({sub, Left, Right} = _Exp, Record) -> {'-', ms(Left, Record), ms(Right, Record)};
ms({mul, Left, Right} = _Exp, Record) -> {'*', ms(Left, Record), ms(Right, Record)};
ms({'div', Left, Right} = _Exp, Record) -> {'div', ms(Left, Record) , ms(Right, Record)};
ms({var, Name} = _Exp, Record) ->
    list_to_atom(lists:concat(["$" | io_lib:format("~p", [ms_util2:get_index(Record, Name)])]));
    %list_to_atom(lists:flatten(io_lib:format("$~p", [ms_util2:get_index(Record, Name)])));
ms(Num, _) when is_number(Num) -> Num;
ms(Atom, _) when is_atom(Atom) -> Atom;

%% Comparison
ms({eq, Left, Right} = _Exp, Record) ->
    {'==', ms(Left, Record), ms(Right, Record)};
ms({ge, Left, Right} = _Exp, Record) ->
    {'>', ms(Left, Record), ms(Right, Record)};
ms({le, Left, Right} = _Exp, Record) ->
    {'<', ms(Left, Record), ms(Right, Record)};
ms({geq, Left, Right} = _Exp, Record) ->
    {'>=', ms(Left, Record), ms(Right, Record)};
ms({leq, Left, Right} = _Exp, Record) ->
    {'=<', ms(Left, Record), ms(Right, Record)};
ms({diff, Left, Right} = _Exp, Record) ->
    {'=/=', ms(Left, Record), ms(Right, Record)};

%% Boolean Expressions
ms({'and', Left, Right} = _Exp, Record) -> {'and', ms(Left, Record), ms(Right, Record)};
ms({'or', Left, Right} = _Exp, Record) -> {'or', ms(Left, Record), ms(Right, Record)};
ms({'not', Expression} = _Exp, Record) -> {'xor', true, ms(Expression, Record)};
ms(true, _) -> true;
ms(false, _) -> false.
