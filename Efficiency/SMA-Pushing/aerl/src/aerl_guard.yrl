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
pred -> exp '<=' exp : {'=<', '$1', '$3'}.
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
exp -> var : v('$1').
exp -> literal : v1('$1').
exp -> self : v('$1').
exp -> const : list_to_atom(v('$1')).

Erlang code.

-export([make/1]).

v({_, _, V}) -> V.
v1({_, _, V}) ->
    put(V,V),
    ms_util2:get_index(aerl_store,V).

make(Tokens) ->
    {ok,Parsed} = parse(Tokens),
    Att = [A || {A,A} <- get()],
    Guard = aerl_ms:make(Parsed),
    [Att,Guard].
