-module(read1).
-compile([export_all]).

%% wread(Filename) when is_list(Filename) ->
%%     {ok, Terms} = file:consult(Filename),
%%     Terms1 = lists:map(fun(X) ->
%% 			       lists:map(fun(Y) ->
%% 						 lists:map(fun(Z) ->
%% 								   list_to_atom("m" ++ integer_to_list(Z)) end, Y) end, X) end, Terms),
%%     WIndex = lists:map(fun(X) ->
%% 			       list_to_atom("w" ++ integer_to_list(X)) end, lists:seq(1,length(Terms))),
%%     List = lists:zip(WIndex, Terms1),
%%     List.

%% mread(Filename) when is_list(Filename) ->
%%     {ok, Terms} = file:consult(Filename),
%%     Terms1 = lists:map(fun(X) ->
%% 			       lists:map(fun(Y) ->
%% 						 lists:map(fun(Z) ->
%% 								   list_to_atom("w" ++ integer_to_list(Z)) end, Y) end, X) end, Terms),
%%     MIndex = lists:map(fun(X) ->
%% 			       list_to_atom("m" ++ integer_to_list(X)) end, lists:seq(1,length(Terms))),
%%     List = lists:zip(MIndex, Terms1),
%%     List.

wread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Terms1 = lists:map(fun(X) ->
			       lists:map(fun(Y) ->
						 lists:map(fun(Z) ->
								   list_to_atom("m" ++ integer_to_list(Z)) end, Y) end, X) end, Terms),
    WIndex = lists:map(fun(X) ->
			       list_to_atom("w" ++ integer_to_list(X)) end, lists:seq(1,length(Terms))),
    List = lists:zipwith(fun(X,Y) -> [{id,X},{prefs,Y}] end, WIndex, Terms1),
    List.

mread(Filename) when is_list(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Terms1 = lists:map(fun(X) ->
			       lists:map(fun(Y) ->
						 lists:map(fun(Z) ->
								   list_to_atom("w" ++ integer_to_list(Z)) end, Y) end, X) end, Terms),
    MIndex = lists:map(fun(X) ->
			       list_to_atom("m" ++ integer_to_list(X)) end, lists:seq(1,length(Terms))),
    List = lists:zipwith(fun(X,Y) -> [{id,X},{prefs,Y}] end, MIndex, Terms1),
    List.
