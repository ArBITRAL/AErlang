-module(ms_util).

-export([make_ms/2,make_rec/2]).

%%%
%%% External Functions (API)
%%%

print(X) ->
    io:format("~p~n",[X]).

make_ms(Rec,List) when is_atom(Rec), is_list(List) ->
    NoFields=ms_util2:no_of_fields(Rec),
    NewList=proc_list(Rec,List),
    Return=list_to_tuple([Rec|build(NewList,NoFields)]),
    Return.

make_rec(Rec,Env) when is_atom(Rec), is_list(Env) ->
    List = proplists:get_keys(Env),
    NoFields=ms_util2:no_of_fields(Rec),
    NewList=proc_list1(Rec,List),
    Inter = build1(NewList,NoFields,Env),
    Return=list_to_tuple([Rec|Inter]),
    Return;
make_rec(Rec,Env) when is_atom(Rec) ->
    List = proplists:get_keys([Env]),
    NoFields=ms_util2:no_of_fields(Rec),
    NewList=proc_list1(Rec,List),
    Inter = build1(NewList,NoFields,[Env]),
    Return=list_to_tuple([Rec|Inter]),
    Return.




%%%
%%% Internal Functions
%%%

proc_list1(Rec,List) -> proc_list1(Rec,List,[]).

%% bit funky - return the list sorted in reverse order
proc_list1(Rec,[],Acc)            -> lists:reverse(lists:keysort(2,Acc));
proc_list1(Rec,[Field|T],Acc) ->
    proc_list1(Rec,T,[{Field,aerl_utils:getIndex(Field,Rec)}|Acc]).

build1(List,NoFields,Data) -> build1(List,NoFields,[],Data).

build1([],0,Acc,_)           -> Acc;
build1([{Name,N}|T],N,Acc,Data)        -> build1(T,N-1,[proplists:get_value(Name,Data)|Acc],Data);
build1([H|T],N,Acc,Data)        -> build1([H|T],N-1,['_'|Acc],Data);%don't drop H - will match later
build1([],N,Acc,Data)           -> build1([],N-1,['_'|Acc],Data).


proc_list(Rec,List) -> proc_list(Rec,List,[]).

%% bit funky - return the list sorted in reverse order
proc_list(Rec,[],Acc)            -> lists:reverse(lists:sort(Acc));
proc_list(Rec,[Field|T],Acc) ->
    proc_list(Rec,T,[aerl_utils:getIndex(Field,Rec)|Acc]).

build(List,NoFields) -> build(List,NoFields,[]).

build([],0,Acc)           -> Acc;
build([N|T],N,Acc)        -> build(T,N-1,[list_to_atom(lists:flatten(io_lib:format("$~p", [N])))|Acc]);
build([H|T],N,Acc)        -> build([H|T],N-1,['_'|Acc]);%don't drop H - will match later
build([],N,Acc)           -> build([],N-1,['_'|Acc]).
