Definitions.

INT = (\+|-)?[0-9]*
OP = \+|\-|\*|\/|\(|\)|(and)|(or)|(not)|(in)|=|<|<=|==|=>|>|<>|\,
ATOM = [a-z][0-9a-zA-Z_]*
VAR = (\$)[A-Z_][0-9a-zA-Z_]*

Rules.

tt          :   {token,{true,TokenLine,TokenChars}}.
ff          :   {token,{false,TokenLine,TokenChars}}.
true        :   {token,{true,TokenLine,TokenChars}}.
false       :   {token,{false,TokenLine,TokenChars}}.
{INT}+      :   {token, {int, TokenLine, TokenChars}}.
{OP}	    :   {token, {list_to_atom(TokenChars), TokenLine}}.

{VAR}	    :    {token, {var, TokenLine, list_to_atom(strip(TokenChars,TokenLen))}}.
{ATOM}	    :   {token, {literal, TokenLine, list_to_atom(TokenChars)}}.
_{ATOM}	        : {token, {const, TokenLine, strip(TokenChars,TokenLen)}}.
(this.){ATOM}	: {token, {self, TokenLine, list_to_atom(strip2(TokenChars,TokenLen))}}.

\s|\n|\t	: skip_token.

Erlang code.
-export([scan/3]).

sub([],_,_) -> [];
sub([{self,Line,Key}=_H|T],Bindings,Tab) ->
    Value = aerl_utils:getAtt(Tab,Key),
    [{self,Line,Value} | sub(T,Bindings,Tab)];
sub([{var,Line,Key}=_H|T],Bindings,Tab) ->
    Value = proplists:get_value(Key, Bindings),
    [{var,Line,Value} | sub(T,Bindings,Tab)];
sub([H|T],Bindings,Tab) ->
    [H | sub(T,Bindings,Tab)].

scan(Pred,Bindings,T) ->
    {ok, Tokens, _} = aerl_scanner:string(Pred),
    sub(Tokens,Bindings,T).

strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 1).

strip2(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 6, TokenLen - 4).
