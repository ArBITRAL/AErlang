-module(aerl_broker).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 a_send/3]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{mode}).

-include("aerl.hrl").

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
    %% wpool:start_pool(
    %%   ?MODULE, [
    %%             {workers, 100},
    %%             {worker, {?SERVER, [Mode]}}
    %%            ]
    %% ).
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mode], []).

a_send(Pred, Msg, Env) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    gen_server:cast(?SERVER, {Pred, Msg, Env}).

s_send(Pred, Msg, Env) ->
    %%wpool:cast(?MODULE,{Pred, Msg, Env, self()}, random_worker).
    gen_server:call(?SERVER, {Pred, Msg, Env}).

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
    {ok, #state{mode=Mode}}.

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
handle_call({Pred,Msg,Env}, From, State) ->
    case State#state.mode of
	default ->
	    spawn(fun() -> default2(Pred,Msg,Env,From,opt2) end)
    end,
    {noreply, State}.



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

handle_cast({Pred,Msg,Env}, State) ->
    case State#state.mode of
	default ->
	    %% Using general handling
	    %% io:format("Using general handling~n"),
	    %%spawn(fun() -> default(Pred,Msg,Env,Pid) end)
	    default(Pred,Msg,Env,opt2)
    end,
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

print(X) ->
    io:format("~p~n",[X]).

%% For handling the asynchronous send
%% Pred is tokens returned from scanner
%% normal send
default(Pred,Msg,Senv,opt) ->
    %%% GENERAL CASE WITH ATTS as collumn names
    [Att,Guard] = aerl_guard:make(Pred),
    RInfo = [pid],
    Head = ms_util:make_ms(aerl_store,RInfo ++ Att),
    Result = [['$1']],
    Recv = mnesia:dirty_select(aerl_store,[
    					   {Head,
    					    [Guard],
    					    Result}
    					  ]),
    [P ! {Msg,ok,Senv} || [P] <- Recv];
default(Pred,Msg,Senv,opt2) ->
    %%FOR EQUALITY and AND/OR case, FAST!
    DNF = aerl_dnf:make(Pred),
    Recv = lists:map(
     fun(Clause) -> %% For each condition ~ sending predicate
	      Rec = ms_util:make_rec(aerl_store,Clause),
	      Procs = mnesia:dirty_match_object(Rec), %% match a list of processes
	      lists:foldl(fun(Process,Sum) ->
	      			  Process#aerl_store.pid ! {Msg,ok,Senv},
	      			  Sum + 1
	      		  end, 0, Procs)
     end,
	     DNF),
    count_msg(lists:sum(Recv)+1).

default2(Pred,Msg,Senv,{Pid,_} = From,opt) ->
    %%% FOR GENERAL CASE WITH VERBOSE attributes as collumn names
    [Att,Guard] = aerl_guard:make(Pred),
    Head = ms_util:make_ms(aerl_store,[pid|Att]),
    Result = [['$1']],
    %%io:format(" ~p send ~p to ~p ~n",[Senv,Msg,Pred]),
    %% print(Head),
    %% print(Guard),
    %% print(Result),
    Recv = mnesia:dirty_select(aerl_store,[
    					   {Head,
    					    [Guard],
    					    Result}
    			 		  ]),
    N = length(Recv),
    gen_server:reply(From,N),
    [P ! {Msg,Pid,Senv} || [P] <- Recv];
default2(Pred,Msg,Senv,{Pid,_}=From,opt2) ->
    %%FOR EQUALITY and AND/OR case, FAST!
    DNF = aerl_dnf:make(Pred),
    Recv = lists:map(
     fun(Clause) ->
	      Rec = ms_util:make_rec(aerl_store,Clause),
	      Procs = mnesia:dirty_match_object(Rec) ,
	     %%io:format("~p send to ~p~n",[Senv,Procs]),
	      lists:foldl(fun(Process,Sum) ->
	      			  Process#aerl_store.pid ! {Msg,Pid,Senv},
	      			  Sum + 1
	      		  end, 0, Procs)
      end,
      DNF),
    count_msg(lists:sum(Recv)+1),
    gen_server:reply(From,lists:sum(Recv)).

dirty_fold(Fun, Acc, Tab) ->
  Key = mnesia:dirty_first(Tab),
  dirty_fold(Fun, Acc, Tab, Key).

dirty_fold(Fun, Ret, Tab, '$end_of_table') ->
  Ret;
dirty_fold(Fun, Acc, Tab, Key) ->
  NxtAcc = Fun(Key, Acc),
  NxtKey = mnesia:dirty_next(Tab, Key),
  dirty_fold(Fun, NxtAcc, Tab, NxtKey).


count_msg(0) -> ok;
count_msg(X) when X > 0 ->
    ets:update_counter(message,num,X);
count_msg(X) ->
    io:format("What is this ~p ~n",[X]).
