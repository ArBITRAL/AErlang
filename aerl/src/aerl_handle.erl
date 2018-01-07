%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2017, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2017 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(aerl_handle).

-behaviour(gen_server).

%% API
-export([start_link/0,add/2,fetch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {msg,ctr}).

%%%===================================================================
%%% API
%%%===================================================================
add(Pid,Msg) ->
    gen_server:cast(Pid, {add,Msg}).

fetch(Pid,Pred) ->
    gen_server:call(Pid, {fetch, Pred}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Dict = [],
    {ok, #state{msg=Dict,ctr=0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({fetch, Pred}, From, State) ->
    List = State#state.msg,
    M =pull_messages(From,Pred,List),
    {SPred,SEnv,Msg} = M,
    {reply, {Msg,{SEnv}}, State#state{msg=List--[M]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add,Msg}, State) ->
    List = State#state.msg,
    %% append Msg to message lists
    {noreply, State#state{msg = [Msg|List]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% pull_messages(From,Pred,List) ->
%%     pull_messages(From,Pred,List,false).

pull_messages(_,_,[]) -> [];
pull_messages(From,Pred,[{Spred,Senv,Msg}=M|List]) ->
    case Pred(Senv) of
	true ->
	    M;
	false ->
	    pull_messages(From,Pred,List)
    end.
