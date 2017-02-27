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


%% bof([],_,_) -> false;
%% bof([H|T],none,M) ->
%%     case lists:member(M,H) of
%% 	true -> true;
%% 	false -> bof(T,none,M)
%%     end;
%% bof([H|T],P,M) ->
%%     case {lists:member(P,H),lists:member(M,H)} of
%% 	{true,_} -> false;
%% 	{false,true} -> true;
%% 	_ -> bof(T,P,M)
%%     end.

%% score(_,none) ->
%%     0;
%% score(L,MAtts) ->
%%     WPrefs = [V || {_,V} <- L],
%%     f(WPrefs,MAtts).

%% f(L1,L2) ->
%%     L = L1 -- L2,
%%     if length(L) == length(L1) -> -1;
%%        true -> length(L1) - length(L)
%%     end.

%% bof(L,P,W,B) ->
%%    score(L,P) < score(L,[W,B]).
