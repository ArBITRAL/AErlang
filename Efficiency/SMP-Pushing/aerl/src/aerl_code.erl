-module(aerl_code).
-compile(export_all).

bof(_,none,_) -> true;
bof([],_,_) -> false; %%both men are not in the list
bof([H|T],P,M) ->
    case {lists:member(P,H),lists:member(M,H)} of
	{true,_} -> false;
	{false,true} -> true;
	_ -> bof(T,P,M)
    end.
