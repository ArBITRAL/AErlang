Definitions.

INT = (\+|-)?[0-9]*
OP = \+|\-|\*|\/|\(|\)|(and)|(or)|(not)|(in)|=|<|<=|==|=>|>|<>
ATOM = [a-z][0-9a-zA-Z_]*
VAR = (\$)[A-Z_][0-9a-zA-Z_]*
FUN = (func)\s.+\(.+\)

Rules.

tt          :   {token,{true,TokenLine,TokenChars}}.
ff          :   {token,{false,TokenLine,TokenChars}}.
true        :   {token,{true,TokenLine,TokenChars}}.
false       :   {token,{false,TokenLine,TokenChars}}.
{INT}+      :   {token, {int, TokenLine, TokenChars}}.
{OP}	    :   {token, {list_to_existing_atom(TokenChars), TokenLine}}.

{FUN}       :   {token,{func,TokenLine,strip3(TokenChars,TokenLen)}}.
{ATOM}	    :   {token, {attribute, TokenLine, list_to_atom(TokenChars)}}.
{VAR}	    :    {token, {var, TokenLine, strip1(TokenChars,TokenLen)}}.
_{ATOM}	        : {token, {const, TokenLine, strip(TokenChars,TokenLen)}}.
(this.){ATOM}	: {token, {self, TokenLine, strip2(TokenChars,TokenLen)}}.

\s|\n|\t	: skip_token.

Erlang code.

-export([scan/3]).

strip3(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 6, TokenLen - 5).
strip2(TokenChars,TokenLen) ->
    list_to_atom(lists:sublist(TokenChars, 6, TokenLen - 4)).
strip1(TokenChars,TokenLen) ->
    list_to_atom(lists:sublist(TokenChars, 2, TokenLen - 1)).
strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 1).

scan(Pred,Bindings,T) ->
    {ok, Tokens, _} = aerl_scanner:string(Pred),
    sub(Tokens,Bindings,T).

sub([],_,_) -> [];
sub([{self,Line,Key}=_H|T],Bindings,Tab) ->
    Value = aerl_utils:getAtt(Tab,Key),
    [{self,Line,Value} | sub(T,Bindings,Tab)];
sub([{var,Line,Key}=_H|T],Bindings,Tab) ->
    Value = proplists:get_value(Key, Bindings),
    [{var,Line,Value} | sub(T,Bindings,Tab)];
sub([H|T],Bindings,Tab) ->
    [H | sub(T,Bindings,Tab)].
