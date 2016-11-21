Nonterminals exps exp.

Terminals
	'+' '-' '*' '/' '(' ')' '>' '<' '<>' '>=' '<=' '=' 'and' 'or' 'not'
	int attribute self const true false func var.

Rootsymbol exps.

Left 100 'or'.
Left 200 'and'.
Right 300 'not'.
Left 400 '>' '<' '<>' '>=' '<=' '='.
Left 500 '+' '-'.
Left 600 '*' '/'.


exps -> exp : '$1'.


exp -> '(' exp ')' : '$2'.
exp -> exp '+' exp : {add, '$1', '$3'}.
exp -> exp '-' exp : {sub, '$1', '$3'}.
exp -> exp '*' exp : {mul, '$1', '$3'}.
exp -> exp '/' exp : {'div', '$1', '$3'}.
exp -> '-' exp : {minus, '$2'}.
exp -> int : list_to_integer(v('$1')).
exp -> true : true.
exp -> false : false.
exp -> var : v('$1').
exp -> self : v('$1').
exp -> const : list_to_atom(v('$1')).

exp -> func : {func,v('$1')}.

exp -> attribute : {var, v('$1')}.

exp -> exp '>=' exp : {geq, '$1', '$3'}.
exp -> exp '<=' exp : {leq, '$1', '$3'}.
exp -> exp '=' exp : {eq, '$1', '$3'}.
exp -> exp '>' exp : {ge, '$1', '$3'}.
exp -> exp '<' exp : {le, '$1', '$3'}.
exp -> exp '<>' exp : {diff, '$1', '$3'}.

exp -> exp 'or' exp : {'or', '$1', '$3'}.
exp -> exp 'and' exp : {'and', '$1', '$3'}.
exp -> 'not' exp : {'not', '$2'}.


Erlang code.

v({_, _, Value}) -> Value.
