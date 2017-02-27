-module(stable).
-export([check/3,test/0]).

check(Matching,WomenList,MenList) ->
    %% detach arg lists [{id,Id},{prefs,Prefs}] into new lists
    NewM = [{Id,Prefs} || [{id,Id},{prefs,Prefs}] <- MenList],
    NewW = [{Id,Prefs} || [{id,Id},{prefs,Prefs}] <- WomenList],
    %% Check first if there one-to-many matching
    Men = proplists:get_keys(Matching),
    Women = [proplists:get_value(M,Matching) || M <- Men],
    N = length(Men),
    N = length(sets:to_list(sets:from_list(Men))),
    N = length(sets:to_list(sets:from_list(Women))),
    %% Check if there any blocking pair
    man_check(Matching,NewW,NewM,Matching).

man_check([],_,_,_) -> true;
man_check([H|T],WL,ML,Matching) ->
    {M,W} = H,
    %%io:format("Check the pair {~p,~p}~n",[M,W]),
    List = get(M,ML),
    MList = list(W,List,[]),
    %%io:format("Women who ~p prefer more ~p~n",[M,MList]),
    case woman_check(MList,M,Matching,WL) of
	pass -> man_check(T,WL,ML,Matching);
	Woman -> io:format("Blocking pair (~p,~p)~n",[M,Woman]),false
    end.

woman_check([],_,_,_) -> pass;
woman_check([H|_T],M,Matching,WL) when is_list(H) ->
    woman_check(H,M,Matching,WL);
woman_check([H|T],M,Matching,WL) ->
    %%io:format("Now woman ~p check~n",[H]),
    Partner = getwpartner(H,Matching),
    %%io:format("Her partner is ~p~n",[Partner]),
    L = get(H,WL),
    case bof(L,Partner,M) of
	true ->
	    woman_check(T,M,Matching,WL);
	false -> H
    end.


bof([], none, _) -> true; %% Accpept the new man
bof([], _, _) -> true; %% Both man is not belong to the list
bof([H|T], P, M) ->
    case {lists:member(P,H), lists:member(M,H)} of
	{true, _} -> true;
	{_, true} -> false;
	_ ->
	    bof(T, P, M)
    end.

getwpartner(_,[]) -> [];
getwpartner(M,[H|T]) ->
    case H of
	{List, M} ->
	    List;
	_ -> getwpartner(M,T)
    end.


get(_,[]) -> [];
get(M,[H|T]) ->
    case H of
	{M, List} ->
	    List;
	_ -> get(M,T)
    end.

list(_,[],Result) -> Result;
list(W,[H|T],Result) when is_list(H) ->
     case lists:member(W,H) of
	 true -> Result;
	 false -> list(W,T,[H] ++ Result)
    end;
list(W,[H|T],Result) ->
    case H of
	W -> Result;
	_ -> list(W,T,[H] ++ Result)
    end.

test() ->
    WomenList = read:wreadlines("women.list"),
    MenList = read:mreadlines("men.list"),
  %  M1 = [{m1,w5},{m2,w1},{m3,w6},{m4,w4},{m5,w2},{m6,w3}],
  %  M2 = [{m1,w3},{m2,w1},{m3,w6},{m4,w2},{m5,w5},{m6,w4}],
    M =[{m50,w57},
          {m81,w64},
          {m20,w5},
          {m8,w52},
          {m49,w43},
          {m108,w86},
          {m5,w76},
          {m55,w104},
          {m106,w83},
          {m104,w30},
          {m31,w22},
          {m1,w92},
          {m88,w70},
          {m68,w31},
          {m51,w88},
          {m89,w98},
          {m69,w10},
          {m101,w109},
          {m98,w110},
          {m100,w39},
          {m58,w55},
          {m86,w29},
          {m76,w37},
          {m10,w51},
          {m29,w2},
          {m84,w51},
          {m70,w46},
          {m6,w49},
          {m9,w90},
          {m45,w35},
          {m43,w82},
          {m38,w84},
          {m71,w11},
          {m74,w94},
          {m83,w107},
          {m59,w36},
          {m103,w81},
          {m87,w54},
          {m24,w113},
          {m109,w114},
          {m92,w79},
          {m12,w73},
          {m80,w56},
          {m37,w26},
          {m30,w60},
          {m3,w40},
          {m14,w101},
          {m41,w19},
          {m7,w102},
          {m95,w14},
          {m119,w65},
          {m35,w4},
          {m44,w91},
          {m23,w60},
          {m75,w118},
          {m90,w72},
          {m67,w16},
          {m102,w98},
          {m85,w15},
          {m53,w89},
          {m19,w93},
          {m13,w41},
          {m120,w68},
          {m111,w11},
          {m57,w59},
          {m62,w87},
          {m93,w119},
          {m40,w58},
          {m27,w54},
          {m39,w41},
          {m48,w3},
          {m110,w45},
          {m4,w13},
          {m28,w77},
          {m77,w38},
          {m25,w34},
          {m33,w62},
          {m82,w32},
          {m65,w12},
          {m16,w95},
          {m22,w9},
          {m54,w42},
          {m112,w17},
          {m56,w44},
          {m99,w100},
          {m64,w85},
          {m32,w112},
          {m79,w36},
          {m117,w27},
          {m97,w53},
          {m72,w105},
          {m2,w50},
          {m15,w116},
          {m21,w75},
          {m116,w69},
          {m73,w7},
          {m17,w68},
          {m113,w48},
          {m107,w117},
          {m36,w120},
          {m94,w28},
          {m60,w42},
          {m46,w96},
          {m96,w66},
          {m42,w18},
          {m114,w111},
          {m11,w8},
          {m52,w24},
          {m47,w80},
          {m118,w78},
          {m26,w63},
          {m115,w108},
          {m66,w94},
          {m91,w97},
          {m18,w33},
          {m78,w74},
          {m34,w47},
          {m63,w21},
          {m61,w6},
          {m105,w61}],

  %  true = check(M1,WomenList,MenList),
  %  true = check(M2,WomenList,MenList),
    true = check(M,WomenList,MenList). %% non pass
