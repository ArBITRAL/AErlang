-module(aman).
-compile({parse_transform,aerl_trans}).
-compile(export_all).

% AbC code for Man
% M := [partner:=top[preference];preference:=preference.tail](propose,this.mid)@(wid=partner).(x=invalid)(x).M
% using map to store environment

init(Env) ->
    spawn(fun() ->
		  start(Env) end).
start(Env) ->
    aerl:register(Env),
    io:format("Man <~p> ~p start!~n",[self(),maps:get(id,Env)]),
    loop(Env).

loop(Env) ->
    #{id := Id, body := Body, wealth := Wealth,
      predicate := Predicate, refusal := Refusal} = Env,
    case Predicate of
	[] -> io:format("Man ~p stopped~n",[Id]),exit(normal);
	_ -> ok
    end,

    [H|T] = Predicate,
    case Refusal of
      [] -> Ps = H;
    	_ -> Ps = string:concat(H, refuse_pred(Refusal))
    end,
  %  Ps = H,
    to(Ps) ! {{propose, Id, Wealth, Body}, Env},
    io:format("Man ~p proposed with ~p ~n", [Id, Ps]),
    from("X = yes or X = no"),
    receive
	{X, Wid} ->
	    case X of
		no ->
   		    io:format("Man ~p got rejected by woman ~p~n", [Id, Wid]),
		    loop(Env#{predicate := T,
			      refusal := refuse_list([Wid],Refusal)});
		yes ->
		    %io:format("Man ~p got woman ~p~n", [Id, Wid]),
		    to("id = $Wid") ! {{confirm},Env},
		    io:format("Man ~p confirm to woman ~p~n", [Id, Wid]),
		    F = fun(Foo) ->
				from("X1 = yes or X1 = no"),
				receive
				    {X1, Other} ->
					case X1 of
					    yes ->
					io:format("Man ~p says sorry to woman ~p because of ~p ~n", [Id, Other, Wid]),
					to("id = $Other") ! {{sorry},Env},Foo(Foo);
					    no ->
						io:format("Man ~p separated to woman ~p~n", [Id, Wid]),
						loop(Env#{predicate := T, refusal:= refuse_list([Wid],Refusal)})
					end
				end
			end,
		    F(F)
	    end
    end.

refuse_list(L1,L2) ->
    L = lists:merge(L1,L2),
    lists:usort(L).

refuse_pred(L) ->
    lists:foldl(fun(X, Sum) -> " and id =/= " ++ atom_to_list(X) ++ Sum end, "", L).
