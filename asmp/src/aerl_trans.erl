%%% Code in this module is written based on the two sources
%%% i) the erl_id module of Erlang OTP 19.
%%% ii) Chap 2.3.3 of the Erlang runtime system book

-module(aerl_trans).
-export([parse_transform/2]).

-define(FUNCTION(Clauses), {function, Label, Name, Arity, Clauses}).
-define(RECEIVE(Line, Clauses), {'receive', Line, Clauses}).
-define(RECEIVE3, {'receive', Line, Clauses}).
-define(RECEIVE5, {'receive', Line, Clauses, To, Co}).

parse_transform(AST, _Options) ->
    io:format("~p~n", [AST]),
    AST1 = ast(AST,[]),
   io:format("AFTER~p~n", [AST1]),
    io:format("AFTER~s~n", [erl_prettypr:format(erl_syntax:form_list(AST1))]),
    AST1.

%% we are only interested in the code inside functions.
ast([?FUNCTION(Clauses) | Elements], Res) ->
    ast(Elements, [?FUNCTION(aerl_clauses(Clauses)) | Res]);
ast([Other | Elements], Res) -> ast(Elements, [Other | Res]);
ast([], Res) -> lists:reverse(Res).

%% we are only interested in the code inside the body of a function.
aerl_clauses([{clause, CLine, A1, A2, Code} | Clauses]) ->
    [{clause, CLine, A1, A2, exprs(Code)} | aerl_clauses(Clauses)];
aerl_clauses([]) -> [].

%% interested in attribute receive
exprs([E0 = {call,_Line,{atom,_Line,from},_}|Es]) ->
    [H|T] = Es,
    E1 = aerl_code(E0,H), % transalation
    lists:append(E1,exprs(T));

%% exprs([E0 = {call,_Line,{atom,_Line,from_ack},_}|Es]) ->
%%     [H|T] = Es,
%%     E1 = aerl_code(E0,H),
%%     lists:append(E1,exprs(T));

%% exprs([E0 = {call,_Line,{atom,_Line,from_a_c},_}|Es]) ->
%%     [H|T] = Es,
%%     E1 = aerl_code(E0,H),
%%     lists:append(E1,exprs(T));

exprs([{op, Line, '!', {call, Line, {atom, Line, to}, _}, _} = E0 | Es]) ->
    E1 = aerl_code(E0),
    [E1 | exprs(Es)];

exprs([E0 | Es]) ->
    E1 = expr(E0),
    [E1 | exprs(Es)];

exprs([]) -> [].

expr({'if', Line, Cs0}) ->
    Cs1 = aerl_clauses(Cs0),
    {'if', Line, Cs1};
expr({'case', Line, E0, Cs0}) ->
    E1 = expr(E0),
    Cs1 = aerl_clauses(Cs0),
    {'case', Line, E1, Cs1};
expr({'receive', Line, Cs0}) ->
    Cs1 = aerl_clauses(Cs0),
    {'receive', Line, Cs1};
expr({'receive', Line, Cs0, To0, ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = aerl_clauses(Cs0),
    {'receive', Line, Cs1, To1, ToEs1};
expr({'try', Line, Es0, Scs0, Ccs0, As0}) ->
    Es1 = exprs(Es0),
    Scs1 = aerl_clauses(Scs0),
    Ccs1 = aerl_clauses(Ccs0),
    As1 = exprs(As0),
    {'try', Line, Es1, Scs1, Ccs1, As1};
expr({match, Line, P0, {op, Line, '!', {call, Line, {atom, Line, to}, [Pred]}, Msg}}) ->
    E1 = parse_aerl_send_count(Pred, Msg),
    {match, Line, P0, E1};
expr(Other) ->
    Other.

-define(ASEND(Pred, Msg), {op, _, '!', {call, _, {atom, _, to}, [Pred]}, Msg}).
-define(ARECV(Pred, Line), {call, _, {atom, Line, from}, [Pred]}).
-define(ARECV_COUNT(Pred, Count, Line), {call, _, {atom, Line, from}, [Pred,Count]}).

%% We look for: to(Pred) ! Msg, which is an attribute send
aerl_code(?ASEND(Pred, Msg)) -> parse_aerl_send(Pred,Msg).

%% We look for: from(Pred), receive ... end, which is an attribute receive
aerl_code(?ARECV(Pred, _), ?RECEIVE3) -> from_syntax(Pred,?RECEIVE3);
aerl_code(?ARECV(Pred, _), ?RECEIVE5) -> from_syntax(Pred,?RECEIVE5);
aerl_code(?ARECV(Pred, Line), _) -> throw({"A 'from' construct must be followed by receive", Line});
 % wrong code convention
aerl_code(?ARECV_COUNT(Pred, Count, _), ?RECEIVE3) -> from_syntax_count(Pred, Count, ?RECEIVE3);
aerl_code(?ARECV_COUNT(Pred, Count, _), ?RECEIVE5) -> from_syntax_count(Pred, Count, ?RECEIVE5);
aerl_code(?ARECV_COUNT(Pred, Count, Line), _) -> throw({"A 'from' construct must be followed by receive", Line}). % wrong code convention


%% parse send primitive
parse_aerl_send({_, Line, _} = Pred, Msg) ->
    Bindings = binding(Pred),
    {call, Line, {remote, Line, {atom, Line, aerl_check}, {atom, Line, local_eval_send}}, [Pred, Bindings, Msg]}.

parse_aerl_send_count({_,Line,_} = Pred, Msg) ->
    Bindings = binding(Pred),
    {call,Line, {remote,Line,{atom,Line,aerl_check},{atom,Line,local_eval_send2}}, [Pred, Bindings, Msg]}.

%% parse receive primitive
parse_aerl_recv({_, Line, _} = Pred, Predname) ->
    Bindings =  binding(Pred),
    {match, Line, Predname, {call, Line, {remote, Line, {atom, Line, aerl_check}, {atom, Line, local_eval_receive}}, [Pred, Bindings]}}.

%% parse receive primitive
parse_aerl_recv_count({_, Line, _} = Pred, PredName, Count) ->
    Bindings =  binding(Pred),
    {match, Line, PredName,{call, Line, {remote, Line, {atom, Line, aerl_check}, {atom, Line, local_eval_receive}}, [Pred, Bindings, Count]}}.

%% from construct by
%% handle this contruct with the receive construct at once
%% THis is for receiving pred alone
from_syntax({_, Line, _} = Pred, RC) ->
    Fname = {var, Line, list_to_atom(lists:append("_F",integer_to_list(Line)))},
    Fooname = {var,Line,list_to_atom(lists:append("_Foo",integer_to_list(Line)))},
    Predname = {var,Line,list_to_atom(lists:append("_Rpred",integer_to_list(Line)))},
    LocalEval = parse_aerl_recv(Pred, Predname),
    RC1 = cons_receive(RC, Predname, Fname, Fooname),
    NewCall =  {call,Line,Fname,[Fname]},
    [LocalEval,RC1,NewCall].

from_syntax_count({_, Line, _} = Pred, Count, RC) ->
    Fname = {var, Line, list_to_atom(lists:append("_F",integer_to_list(Line)))},
    Predname = {var, Line, list_to_atom(lists:append("_Rpred",integer_to_list(Line)))},
    LocalEval = parse_aerl_recv(Pred, Predname),
    RC1 = cons_receive_count(RC, Predname, Fname, Count),
    [LocalEval | RC1].


cons_receive({_, Line, C0} , Predname, Fname, Fooname) ->
    {match, Line, Fname,
     {'fun', Line, {clauses,
       [{clause, Line, [Fooname],
	 [],
	 [{'receive', Line, with_insert_push_ack(C0, Predname, Fooname)}]
	}]}}};
cons_receive({_, Line, C0, To, ToCs} , Predname, Fname, Fooname) ->
    {match, Line, Fname,
     {'fun', Line, {clauses,
       [{clause, Line, [Fooname],
	 [],
	 [{'receive', Line, with_insert_push_ack(C0, Predname, Fooname), To, exprs(ToCs)}]
	}]}}}.

cons_receive_count({_, L1, C0}, PredName, {_, Line, _} = Fname, Count) ->
    Num = list_to_atom(lists:append("_Num",integer_to_list(Line))),
    FooName = list_to_atom(lists:append("_Foo",integer_to_list(Line))),
    Basecase = {clause,L1, [{integer, L1, 0}], [], [{atom, L1, ok}]},
    RCall =  {call, L1, {var, L1, FooName}, [{op, L1, '-', {var,L1,Num}, {integer,L1,1}}]},
    RCall2 =  {call, L1, {var, L1, FooName}, [{var,L1,Num}]},
    NoReply = {clause, L1, [{atom, L1, aerl_no_reply}], [], [RCall]}, % disable no_reply message
    RC1 = {match,L1, Fname,
		  {named_fun, L1, FooName,
		   [Basecase,
		    {clause, L1, [{var,L1,Num}], [],
		     [{'receive',L1, [NoReply | with_insert_push_ack_count(C0,PredName,RCall,RCall2)]}]}]}},
    NCall = {call,L1,Fname,[Count]},
    [RC1,NCall];
cons_receive_count({_, L1, C0, To, ToCs}, PredName, Fname, Count) ->
    Num = list_to_atom(lists:append("_Num",integer_to_list(L1))),
    FooName = list_to_atom(lists:append("_Foo",integer_to_list(L1))),
    BaseCase = {clause,L1, [{integer, L1, 0}], [], [{atom, L1, ok}]},
    RCall =  {call, L1, {var, L1, FooName}, [{op, L1, '-', {var,L1,Num}, {integer,L1,1}}]},
    RCall2 =  {call, L1, {var, L1, FooName}, [{var,L1,Num}]},
    NoReply = {clause, L1, [{atom, L1, aerl_no_reply}], [], [RCall]}, % disable no_reply message
    RC1 = {match, L1,  Fname,
		  {named_fun, L1, FooName,
		   [BaseCase,
		    {clause, L1, [{var,L1,Num}], [],
		     [{'receive', L1, [NoReply | with_insert_push_ack_count(C0,PredName,RCall,RCall2)], To, exprs(ToCs)}]}]}},
    NCall = {call, L1, Fname, [Count]},
    [RC1,NCall].

%%% helper functions

%% build list of Bindings for variables in the form $X
binding({string, Line, Pred}) ->
    stringify(Line, Pred);
binding({_,Line,_}) ->
    {nil,Line}.

stringify(Line, Pred) ->
    Rx = "[\\$|\\?](\\w+)",
    VarsNames =
	case re:run(Pred, Rx, [global, {capture, all_but_first, list}]) of
	    {match, Values} -> lists:concat(Values);
	    _ -> []
	end,
    Fun = fun(VarName) ->
		{tuple,Line,[{atom,Line,list_to_atom(VarName)},{var,Line,list_to_atom(VarName)}]}
	  end, % a closure
    VarsForms = lists:map(Fun, VarsNames),
    Bindings = cons_form(Line, VarsForms),
    Bindings.

cons_form(Line, []) -> {nil, Line};
cons_form(Line, [First | Rest]) -> {cons, Line, First, cons_form(Line, Rest)}.

%% from
with_insert_push_ack([],_,_) -> [];
with_insert_push_ack([H|T],Converted,Fooname) ->
    H1 = with_modify_push_ack(H,Converted,Fooname),
    [H1 | with_insert_push_ack(T,Converted,Fooname)].

with_modify_push_ack({clause,Line,[Vars],Arr,List},PredName,Fooname) ->
    Decoration = {var,Line,list_to_atom(lists:append("_Decoration",integer_to_list(Line)))},
    NewVars = {tuple,Line,[Vars,Decoration]},
    BrokerCheck = {call,Line,
		     {remote,Line,{atom,Line,aerl_check},{atom,Line,check}},
		     [PredName,Decoration]},
    NewList = [{'case',Line, BrokerCheck,
	  [{clause,Line,
	    [{atom,Line,true}],
	    [],
	    exprs(List)},
	   {clause,Line, [{atom,Line,false}],
	    [],
	    [{call,Line,Fooname,[Fooname]}]}]}],
     {clause,Line,[NewVars],Arr,NewList}.

%% from count
with_insert_push_ack_count([],_,_,_) ->
    [];
with_insert_push_ack_count([H|T], PredName, RCall,RCall2) ->
    H1 = with_modify_push_ack_count(H, PredName, RCall,RCall2),
    [H1 | with_insert_push_ack_count(T, PredName, RCall,RCall2)].

with_modify_push_ack_count({clause, Line, [Vars], Arr, Code},PredName, RCall, RCall2) ->
    Decoration = {var,Line,list_to_atom(lists:append("_Decoration",integer_to_list(Line)))},
    NewVars = {tuple,Line,[Vars,Decoration]},
    BrokerCheck = {call,Line,
		     {remote,Line,{atom,Line,aerl_check},{atom,Line,check}},
		     [PredName,Decoration]},
    NewList =
	[{'case',Line,
	   BrokerCheck,
	  [{clause,Line,
	    [{atom,Line,true}],
	    [],
	    lists:append(exprs(Code),[RCall])},
	   {clause,Line,
	    [{atom,Line,false}],
	    [],
	    [RCall2]}]}],
     {clause,Line,[NewVars],Arr,NewList}.
