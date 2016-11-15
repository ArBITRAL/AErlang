-module(ms_util).

-export([make_ms/2]).

%%%
%%% External Functions (API)
%%%

make_ms(Rec,List) when is_atom(Rec), is_list(List) ->
    NoFields=ms_util2:no_of_fields(Rec),
    NewList=proc_list(Rec,List),
    Return=list_to_tuple([Rec|build(NewList,NoFields)]),
    Return.

%%%
%%% Internal Functions
%%%

proc_list(Rec,List) -> proc_list(Rec,List,[]).

%% bit funky - return the list sorted in reverse order
proc_list(Rec,[],Acc)            -> lists:reverse(lists:sort(Acc));
proc_list(Rec,[Field|T],Acc) ->
    proc_list(Rec,T,[ms_util2:get_index(Rec,Field)|Acc]).

build(List,NoFields) -> build(List,NoFields,[]).

build([],0,Acc)           -> Acc;
build([N|T],N,Acc)        -> build(T,N-1,[list_to_atom(lists:flatten(io_lib:format("$~p", [N])))|Acc]);
build([H|T],N,Acc)        -> build([H|T],N-1,['_'|Acc]);%don't drop H - will match later
build([],N,Acc)           -> build([],N-1,['_'|Acc]).
