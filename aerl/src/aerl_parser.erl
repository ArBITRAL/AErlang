-module(aerl_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/aerl_parser.yrl", 48).

-export([eval/3]).

v({_, _, Value}) -> Value.
v1({_, _, Value}) ->
    put(Value,Value),
    Value.

eval(Pred,Bindings,T) ->
    Tokens =  aerl_scanner:scan(Pred,Bindings,T),
    {ok,Parsed} = parse(Tokens),
    Att = [X || X <- get()],
    [Att,Parsed].

-file("/usr/local/lib/erlang/lib/parsetools-2.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/aerl_parser.erl", 192).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_cont_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, self, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_2(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_3: see yeccpars2_0

yeccpars2_4(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_pred(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_8(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_9: see yeccpars2_0

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_pred(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_14: see yeccpars2_0

%% yeccpars2_15: see yeccpars2_0

yeccpars2_16(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_18: see yeccpars2_4

yeccpars2_19(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_arg_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
yeccpars2_20(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_exp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_23: see yeccpars2_4

%% yeccpars2_24: see yeccpars2_4

%% yeccpars2_25: see yeccpars2_4

%% yeccpars2_26: see yeccpars2_4

%% yeccpars2_27: see yeccpars2_4

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_arg_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_31(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_36: see yeccpars2_4

%% yeccpars2_37: see yeccpars2_4

%% yeccpars2_38: see yeccpars2_4

%% yeccpars2_39: see yeccpars2_4

%% yeccpars2_40: see yeccpars2_4

%% yeccpars2_41: see yeccpars2_4

%% yeccpars2_42: see yeccpars2_4

yeccpars2_43(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_pred(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_arg_list(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arg_list(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_exp(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pred(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pred(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pred(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pred(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pred(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_5_/1}).
-file("src/aerl_parser.yrl", 42).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { const , list_to_atom ( v ( __1 ) ) }
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("src/aerl_parser.yrl", 22).
yeccpars2_6_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/aerl_parser.yrl", 38).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   list_to_integer ( v ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("src/aerl_parser.yrl", 40).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   v1 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/aerl_parser.yrl", 41).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { const , v ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("src/aerl_parser.yrl", 21).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/aerl_parser.yrl", 39).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { const , v ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/aerl_parser.yrl", 26).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'not' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/aerl_parser.yrl", 24).
yeccpars2_16_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'or' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("src/aerl_parser.yrl", 25).
yeccpars2_17_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'and' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("src/aerl_parser.yrl", 30).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("src/aerl_parser.yrl", 40).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   v1 ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("src/aerl_parser.yrl", 29).
yeccpars2_22_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { func , v ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("src/aerl_parser.yrl", 36).
yeccpars2_28_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'div' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("src/aerl_parser.yrl", 34).
yeccpars2_29_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '-' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("src/aerl_parser.yrl", 31).
yeccpars2_30_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("src/aerl_parser.yrl", 33).
yeccpars2_31_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '+' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("src/aerl_parser.yrl", 35).
yeccpars2_32_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '*' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("src/aerl_parser.yrl", 37).
yeccpars2_33_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { '-' , __2 }
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("src/aerl_parser.yrl", 13).
yeccpars2_35_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/aerl_parser.yrl", 27).
yeccpars2_43_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { in , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/aerl_parser.yrl", 14).
yeccpars2_44_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/aerl_parser.yrl", 17).
yeccpars2_45_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '>' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/aerl_parser.yrl", 16).
yeccpars2_46_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '==' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/aerl_parser.yrl", 19).
yeccpars2_47_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '=/=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/aerl_parser.yrl", 15).
yeccpars2_48_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '<=' , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("src/aerl_parser.yrl", 18).
yeccpars2_49_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '<' , __1 , __3 }
  end | __Stack].


-file("src/aerl_parser.yrl", 62).
