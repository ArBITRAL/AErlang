Nonterminals pred exp arg_list.

Terminals
	'+' '-' '*' '/' '(' ')' '>' '<' '<>' '>=' '<=' '=' 'and' 'in' 'or' 'not' ','
	literal int self const true false var.

Rootsymbol pred.

Left 100 'or'.
Left 200 'and'.
Right 300 'not'.
Left 350 'in'.
Left 400 '>' '<' '<>' '>=' '<=' '='.
Left 500 '+' '-'.
Left 600 '*' '/'.

pred -> '(' pred ')' : '$2'.
pred -> exp '>=' exp : {'>=', '$1', '$3'}.
pred -> exp '<=' exp : {'<=', '$1', '$3'}.
pred -> exp '=' exp : {'==', '$1', '$3'}.
pred -> exp '>' exp : {'>', '$1', '$3'}.
pred -> exp '<' exp : {'<', '$1', '$3'}.
pred -> exp '<>' exp : {'=/=', '$1', '$3'}.

pred -> true : true.
pred -> false : false.

pred -> pred 'or' pred : {'or', '$1', '$3'}.
pred -> pred 'and' pred : {'and', '$1', '$3'}.
pred -> 'not' pred : {'not', '$2'}.
pred -> exp 'in' exp : {'in', '$1', '$3'}.

pred -> literal '(' arg_list ')' : {func,v('$1'),'$3'}.
arg_list -> exp : ['$1'].
arg_list -> exp ',' arg_list : ['$1' | '$3'].

exp -> exp '+' exp : {'+', '$1', '$3'}.
exp -> exp '-' exp : {'-', '$1', '$3'}.
exp -> exp '*' exp : {'*', '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.
exp -> '-' exp : {'-', '$2'}.
exp -> int : list_to_integer(v('$1')).
exp -> var : {const,v('$1')}.
exp -> literal : v1('$1').
exp -> self : {const,v('$1')}.
exp -> const : {const,list_to_atom(v('$1'))}.

Erlang code.

-export([eval/3]).

v({_, _, Value}) -> Value.
v1({_, _, Value}) ->
    put(Value,Value),
    Value.

eval(Pred,Bindings,T) ->
    Tokens =  aerl_scanner:scan(Pred,Bindings,T),
    {ok,Parsed} = parse(Tokens),
    Att = [X || X <- get()],
    [Att,Parsed].
