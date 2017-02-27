-module(read).
-export([wreadlines/1,mreadlines/1]).

wreadlines(Filename) when is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, List} = parse(Bin,"w"),
    [[{id,X},{prefs,Y}] || {X,Y} <- List].


mreadlines(Filename) when is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, List} = parse(Bin,"m"),
    [[{id,X},{prefs,Y}] || {X,Y} <- List].

parse(Bin,Gender) when is_binary(Bin) ->
    parse(Bin, [], [], [], 1, Gender).

parse(<<$\s, Rest/binary>>, Field, Line, Acc, Count, Gender) ->
    parse(Rest, [], [lists:reverse(Field)|Line], Acc, Count, Gender);
parse(<<$\r, Rest/binary>>, Field, Line, Acc, Count, Gender) ->
    parse(Rest, Field, Line, Acc, Count, Gender);
parse(<<$\n, Rest/binary>>, Field, Line, Acc, Count, Gender) ->
    Field1 = lists:reverse(Field),
    FieldList = lists:reverse([Field1|Line]),
    New = split(FieldList,Gender),
    Index = list_to_atom(Gender++ integer_to_list(Count)),
    parse(Rest, [], [], [{Index,New}|Acc], Count+1, Gender);
parse(<<Char, Rest/binary>>, Field, Line, Acc, Count, Gender) ->
    parse(Rest, [Char|Field], Line, Acc, Count, Gender);
parse(<<>>, [], [], Acc, _,_) ->
    {ok, lists:reverse(Acc)};
parse(<<>>, Field, Line, Acc, Count, Gender) ->
    parse(<<$\n>>, Field, Line, Acc, Count, Gender).


split(List,"m") ->
    split(List,[],"w");
split(List,"w") ->
    split(List,[],"m").

split([],Global,_) -> lists:reverse(Global);
split([[]],Global,_Opp) -> lists:reverse(Global);
split([H|T],Global,Opp) ->
    case string:str(H,"-") > 0 of
	false ->
	    %% processing tail T, to grab list
	    {New,Result} = handle(T,list_to_atom(Opp++H),Opp),
	    split(New,[Result|Global],Opp);
	true ->
	    Global
    end.

handle([], Result,_) when is_list(Result) -> {[],Result};
handle([], Result,_) -> {[],[Result]};
handle([H|T], Result,Opp) ->
    case string:str(H,"-") > 0 of
	false ->
	    case is_list(Result) of
		false -> {[H|T], [Result]};
		true -> {[H|T], Result}
	    end;
	true ->
	    H1 = H -- "-",
	    case is_list(Result) of
		true -> handle(T, Result ++ [list_to_atom(Opp++H1)], Opp);
		false -> handle(T, [Result,list_to_atom(Opp++H1)], Opp)
	    end
    end.
