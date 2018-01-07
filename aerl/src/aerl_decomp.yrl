Nonterminals exps exp.

Terminals
	'+' '-' '*' '/' '(' ')' '>' '<' '<>' '>=' '<=' '=' 'and' 'or' 'in' 'not'
	int attribute self const true false func var.

Rootsymbol exps.

Left 100 'or'.
Left 200 'and'.
Left 200 'in'.
Right 300 'not'.
Left 400 '>' '<' '<>' '>=' '<=' '='.
Left 500 '+' '-'.
Left 600 '*' '/'.


exps -> exp : '$1'.

exp -> '(' exp ')' : '$2'.
exp -> exp '+' exp : {'+', '$1', '$3'}.
exp -> exp '-' exp : {'-', '$1', '$3'}.
exp -> exp '*' exp : {'*', '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.
exp -> '-' exp : {'-', '$2'}.
exp -> int : list_to_integer(v('$1')).
exp -> true : true.
exp -> false : false.
exp -> var : v('$1').
exp -> self : v('$1').
exp -> const : list_to_atom(v('$1')).

exp -> func : {func,v('$1')}.

exp -> attribute : v('$1').

exp -> exp '>=' exp : {'>=', '$1', '$3'}.
exp -> exp '<=' exp : {'<=', '$1', '$3'}.
exp -> exp '=' exp : {'==', '$1', '$3'}.
exp -> exp '>' exp : {'>', '$1', '$3'}.
exp -> exp '<' exp : {'<', '$1', '$3'}.
exp -> exp '<>' exp : {'=/=', '$1', '$3'}.

exp -> exp 'or' exp : {'or', '$1', '$3'}.
exp -> exp 'and' exp : {'and', '$1', '$3'}.
exp -> exp 'in' exp : {'in', '$1', '$3'}.
exp -> 'not' exp : {'not', '$2'}.


Erlang code.

-export([make/1]).

v({_, _, Value}) -> Value.

make(Tokens) ->
    {ok, Parsed} = aerl_decomp:parse(Tokens),
    Guards = eval(Parsed),
    Guards.

eval({'and', Left, Right}) ->
    [eval(Left),eval(Right)];
eval({'or', Left, Right}) ->
    eval(Left) ++ eval(Right);
eval({'in',Left,Right}) ->
    [{Left,build(Right)}];
eval({'==',Left,Right}) ->
    [{Left,[Right]}];
eval(Other) -> Other.

build(L) when is_list(L) ->
    L;
build(L) -> [L].
