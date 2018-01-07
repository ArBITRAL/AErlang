-module(aerl_ms).
-export([make/1]).

make(Parsed) ->
    ms(Parsed).

%% Arithmic Expressions
ms({add, Left, Right} = _Exp) -> {'+', ms(Left), ms(Right)};
ms({sub, Left, Right} = _Exp) -> {'-', ms(Left), ms(Right)};
ms({mul, Left, Right} = _Exp) -> {'*', ms(Left), ms(Right)};
ms({'div', Left, Right} = _Exp) -> {'div', ms(Left) , ms(Right)};
ms({attribute, Name} = _Exp) ->
    ms_util2:get_variable(aerl_store, Name);
%% Comparison
ms({eq, Left, Right} = _Exp) ->
    {'==', ms(Left), ms(Right)};
ms({ge, Left, Right} = _Exp) ->
    {'>', ms(Left), ms(Right)};
ms({le, Left, Right} = _Exp) ->
    {'<', ms(Left), ms(Right)};
ms({geq, Left, Right} = _Exp) ->
    {'>=', ms(Left), ms(Right)};
ms({leq, Left, Right} = _Exp) ->
    {'=<', ms(Left), ms(Right)};
ms({diff, Left, Right} = _Exp) ->
    {'=/=', ms(Left), ms(Right)};

%% Boolean Expressions
ms({'and', Left, Right} = _Exp) -> {'and', ms(Left), ms(Right)};
ms({'or', Left, Right} = _Exp) -> {'or', ms(Left), ms(Right)};
ms({'in', Left, Right} = _Exp) ->
    make2(ms(Left), ms(Right));
ms({'not', Expression} = _Exp) -> {'xor', true, ms(Expression)};
ms(Num) when is_number(Num) -> Num;
ms(Atom) when is_atom(Atom) -> Atom;
ms(Tuple) when is_tuple(Tuple) -> Tuple;
ms(List) when is_list(List) -> List.


make2(Name,List) when is_list(List),length(List) > 1 ->
    [H|T] = List,
    {'or',ms({eq,Name,H}),make2(Name,T)};
make2(Name,List) when is_list(List),length(List) == 1 ->
    [Singleton] = List,
    ms({eq,Name,Singleton});
make2(Name,H) ->
    ms({eq,Name,H}).
