%%%-------------------------------------------------------------------
%%% @author Tan Duong <dn.nhattan@gmail.com>
%%% @copyright (C) 2016, Tan Duong
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2016 by Tan Duong <dn.nhattan@gmail.com>
%%%-------------------------------------------------------------------
-module(aerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Mode) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Mode]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Mode]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 4,
		 period => 3600},

    RegistryChild = #{id => aerl_registry,
	       start => {aerl_registry, start_link, [Mode]},
	       restart => permanent,
	       shutdown => 2000,
	       type => worker,
	       modules => [aerl_registry]},
    BrokerChild = #{id => aerl_broker,
	       start => {aerl_broker, start_link, [Mode]},
	       restart => permanent,
	       shutdown => 2000,
	       type => worker,
	       modules => [aerl_broker]},

    {ok, {SupFlags, [RegistryChild,BrokerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
