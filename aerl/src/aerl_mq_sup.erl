%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2017, tan duong
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2017 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(aerl_mq_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->

    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 0,
		 period => 10},

    AChild = #{id => aerl_handle,
	       start => {aerl_handle, start_link, []},
	       restart => temporary,
	       shutdown => 2000,
	       type => worker,
	       modules => [aerl_handle]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
