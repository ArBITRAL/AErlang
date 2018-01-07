-module(aerl_code).
-compile(export_all).

score(_,[]) ->
    -1;
score(L,[W,B]) ->
    X = proplists:get_value(wealth,L),
    Y = proplists:get_value(body,L),
    case {X,Y} of
	{W,B} -> 3;
	{W,_} -> 2;
	{_,B} -> 1;
	_ -> 0
    end.

%% bof(L,P,M) ->
%%    score(L,P) < score(L,M).

bof(L,P,W,B) ->
   score(L,P) < score(L,[W,B]).

mscore(_,[]) ->
    -1;
mscore(L,[W,B]) ->
    X = proplists:get_value(eyes,L),
    Y = proplists:get_value(hair,L),
    case {X,Y} of
	{W,B} -> 3;
	{W,_} -> 2;
	{_,B} -> 1;
	_ -> 0
    end.

mbof(L,P,E,H) ->
   mscore(L,P) < mscore(L,[E,H]).
