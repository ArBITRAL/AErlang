-module(aerl_registry).

-behaviour(gen_server).

-include("aerl.hrl").
%% API
-export([start_link/1,
	 register/1,register/2,
	 unregister/0,unregister/1,
	 update_att/1, update_pred/2]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mode}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Mode) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Mode) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mode], []).

register(Env) ->
    gen_server:call(?SERVER, {register, self(), Env}).
    %aerl_env:setAtt(aerl_working_mode, Mode).

register(Pid, Env) ->
    %aerl_env:initEnv(Env),
    gen_server:call(?SERVER,{register, Pid, Env}).
    %aerl_env:setAtt(aerl_working_mode, Mode).

unregister() ->
    gen_server:call(?SERVER, {unregister, self()}).

unregister(Pid) ->
    gen_server:call(?SERVER, {unregister, Pid}).

%% Other API for environment and predicate updates
%% update attributes
update_att(Env) ->
    gen_server:call(?SERVER, {update_att, Env}).

%% update predicates
update_pred(Pred, Pid) when is_list(Pred) ->
    gen_server:cast(?SERVER, {update_pred, Pred, Pid});
update_pred(Pred, Pid) when is_tuple(Pred) ->
    String = put(abc_function,Pred),
    case Pred == String of
    	false ->
	    gen_server:cast(?SERVER, {update_pred, Pred, Pid});
    	true ->
    	    []
    end.

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
init([Mode]) ->
    process_flag(trap_exit, true), %% faster than erlang:monitor/2
    %% Test read index key
    mnesia:create_table(aerl_store,
     			[{attributes,record_info(fields,aerl_store)}]),
    mnesia:create_table(aerl_pub,
     			[{attributes,record_info(fields,aerl_pub)}]),
    mnesia:create_table(aerl_sub,
     			[{attributes,record_info(fields,aerl_sub)}]),
    {ok, #state{mode = Mode}}.

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
handle_call({register, Pid, Env}, From, State) ->
    spawn(fun() -> register_handle(Pid,Env,From) end),
    {noreply, State};

%% handle_call({register, Pid, Env}, _From, State) ->
%%     Reply = case mnesia:dirty_read(aerl_store, Pid) of
%% 		[_Process] ->
%% 		    {error, pid_already_registered};
%% 		_ ->
%% 		    F = fun(_) -> true end,
%% 		    mnesia:dirty_write(#aerl_store{pid = Pid,
%% 						   id = maps:get(id,Env),
%% 						   env = Env,
%% 						   pred = F}),
%% 		    link(Pid)
%% 	    end,
%%     {reply, Reply, State};
handle_call({unregister, Pid}, _From, State) ->
    unlink(Pid),
    Reply = case mnesia:dirty_read(aerl_store, Pid) of
		[_Process] ->
		    mnesia:dirty_delete(aerl_store, Pid),
		    mnesia:dirty_delete(aerl_sub, Pid),
		    mnesia:dirty_delete(aerl_pub, Pid),
		    ok;
		_ -> {error, undefined}
	    end,
    {reply, Reply, State};
handle_call({update_att, Env}, {Pid, _} = _From, State) ->
    Rec = ms_util:make_rec(aerl_store,Env++[{pid,Pid}]),
    Reply = case mnesia:dirty_read(aerl_store, Pid) of
		[_Process] -> mnesia:dirty_write(Rec),
			     ok;
		_ -> {error, undefined}
	    end,
    {reply, Reply, State}.
%% handle_call({update_pred, Pred}, {Pid, _}=From, State) ->
%%     Reply = case mnesia:dirty_read(aerl_store, Pid) of
%% 	[Process] -> mnesia:dirty_write(Process#aerl_store{pred = Pred}),
%% 		     ok;
%% 	_ -> error
%%     end,
%%     {reply, Reply, State}.
    %%gen_server:reply(From, State).



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
handle_cast({update_pred, Pred, Pid}, State) ->
    %%io:format("update pred ~p~n",[Pred]),
    spawn(fun() -> predicate_handle(Pred,Pid) end),
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
handle_info({'EXIT', Pid, _Why}, State) ->
    mnesia:dirty_delete(aerl_store, Pid),
    {noreply, State};
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
register_handle(Pid, Env, From) when is_map(Env) ->
    register_handle(Pid,maps:to_list(Env), From);
register_handle(Pid, Env, From) when is_list(Env) ->
    NewEnv = [{pid,Pid}]++Env,
    Rec1 = ms_util:make_rec(aerl_store,NewEnv),
%    Rec2 = ms_util:make_rec(aerl_pub,NewEnv),
    %%io:format("To be registered ~p~n",[Rec,
    Reply = case mnesia:dirty_read(aerl_store, Pid) of
		[_Process] ->
		    {error, pid_already_registered};
		_ ->
		    mnesia:dirty_write(Rec1),
%		    mnesia:dirty_write(Rec2),
		    %Now = now_to_micro_seconds(os:timestamp()),
		    %mnesia:dirty_write(#aerl_store{pid=Pid}),
		    %%io:format("~p~n",[Pid]),
		    %%io:format("~p~n",[Now]),
		    link(Pid),
		    ok
    end,
    gen_server:reply(From,Reply).

atom_from_Pid(Pid) ->
    L = pid_to_list(Pid),
    list_to_atom(L).

predicate_handle(Pred,Pid) ->
    Fun = aerl_check:make_fun(Pred),
    NewEnv=[{pid,Pid},{pred,Fun}],
    Rec1 = ms_util:make_rec(aerl_sub,NewEnv),
    mnesia:dirty_write(Rec1),
    %io:format("~p~n",[Fun]),
    %% case mnesia:dirty_read(aerl_sub, Pid) of
    %% 	[Process] ->
    %% 	    mnesia:dirty_write(Process#aerl_sub{pred = Fun});
    %% 	_ -> ok
    %% end.
    ok.

seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.
