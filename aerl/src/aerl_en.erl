%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2017, tan duong
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2017 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(aerl_en).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 initEnv/1,
	 newA/2,
	 setA/2,
	 getA/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {env}).

%%%===================================================================
%%% API
%%%===================================================================
initEnv(Env) ->
    aerl_env_sup:start_child(Env).

newA(A,V) ->
    gen_server:call(?SERVER,{newA,A,V}).

setA(A,V) ->
    gen_server:call(?SERVER,{setA,A,V}).

getA(A) ->
    gen_server:call(?SERVER,{getA,A}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [Env], []).

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
init([Env]) ->
    Dict = dict:new(),
    NewDict = store(Env,Dict),
    {ok, #state{env=NewDict}}.

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
handle_call({newA,A,V}, _From, State) ->
    Reply = ok,
    Dict=State#state.env,
    {reply, Reply, State#state{env=dict:store(A,V,Dict)}};
handle_call({setA,A,V}, _From, State) ->
    Reply = ok,
    Dict=State#state.env,
    {reply, Reply, State#state{env=dict:store(A,V,Dict)}};
handle_call({getA,A}, _From, State) ->
    Dict=State#state.env,
    Reply=dict:find(A,Dict),
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
store([{K,V}|T],Dict) ->
    New = dict:store(K,V,Dict),
    store(T, New);
store([],Dict) -> Dict.
