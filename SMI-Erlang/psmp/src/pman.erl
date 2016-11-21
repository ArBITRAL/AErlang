-module(pman).
-compile(export_all).
-record(state,{id,partner,sex,prefs}).

init(Id,Prefs) ->
    spawn(?MODULE,start,[Id,Prefs]).
start(Id,Prefs) ->
    State = #state{id = Id, sex = man, partner = none, prefs = Prefs},
    receive
	start ->
	    loop(State)
    end.

loop(#state{prefs=[]}) ->
    ok;
%% propose and wait for response
loop(State=#state{prefs=[H|T],id=Id}) ->
    global:whereis_name(H) ! {propose,Id},
    receive
	{no, H} ->
	    loop(State#state{prefs=T})
    end.

%% wait
%% loop(State=#state{id=Id, partner = Partner},MyRank) ->
%%     receive
%% 	{propose, MyRank, Wid} ->
%% 	    happy(Id); %% quit
%% 	{propose, TheirRank, Wid} ->
%% 	    case bof(List,Partner,Wid) of
%% 		false ->
%% 		    global:whereis_name(Wid) ! {no,Id};
%% 		_ ->
%% 		    loop(State,My),
%% 	    end;
%% 	{no, H} ->
%% 	    loop(State#state{prefs=T,partner=none},MyRank+1)
%%     end;

happy(Id) ->
    receive
	{propose,_,Woman} ->
	    global:whereis_name(Woman) ! {no,Id},
	    happy(Id)
    end.

bof(_, none, _) -> true;
bof([], _, _) -> false;
bof([H|T], Partner, Y) ->
  case H of
    Y -> true;
    Partner -> false;
      _ -> bof(T, Partner, Y)
  end.
