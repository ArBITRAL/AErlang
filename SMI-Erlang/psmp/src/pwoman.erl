-module(pwoman).
-compile(export_all).
-record(state, {id, partner, sex, prefs}).

init(Id,Prefs) ->
    spawn(?MODULE,start, [Id,Prefs]).

start(Id,Prefs) ->
    State = #state{id = Id, partner = none, sex = woman, prefs = Prefs},
    receive
	start -> loop(State)
    end.

loop(State=#state{prefs=List,id=Id,partner = Partner}) ->
    receive
	{propose, Man}  ->
	    case bof(List,Partner,Man) of
		true ->
		    global:whereis_name(log) ! {Man,Id},
		    case Partner =/= none of
			true ->
			     global:whereis_name(Partner) ! {no, Id};
			false -> ok
		    end,
		    loop(State#state{partner=Man});
		false ->
		    global:whereis_name(Man) ! {no, Id},
		    loop(State)
	    end
    end.
%% loop(State = #state{id = Id, prefs = Prefs,partner = Partner},[H|T]) ->
%%     global:whereis_name(H) ! {propose, Id}
%%     receive
%% 	{propose, TheirRank, Man} ->
%% 	    case bof(Prefs, Partner, Man) of
%% 		true ->
%% 		    global:whereis_name(log) ! {Man,Id},
%% 		    case Partner =/= none of
%% 			true ->
%% 			    global:whereis_name(Partner) ! {no, Id};
%% 			false -> ok
%% 		    end,
%% 		    MyMan = better_man(Man,Prefs),
%% 		    global:whereis_name(MyMan) ! {propose, Id},
%% 		    loop(State#state{partner = Man});
%% 		false ->
%% 		    global:whereis_name(Man) ! {no, Id},
%% 		    loop(State)
%% 	    end
%%     end.


get_rank(M,List) ->
    get_rank(M,List,1).

get_rank(M,[H|T],Rank) ->
    case M of
	H -> Rank;
	_ -> get_rank(M,T,Rank+1)
    end.
better_man(M,List) ->
    Index = get_rank(M,List),
    lists:nth(Index,List).


bof(_, none, _) -> true;
bof([], _, _) -> false;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
      _ -> bof(T, Partner, Y)
  end.
