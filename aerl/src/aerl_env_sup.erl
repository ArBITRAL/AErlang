-module(aerl_env_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/1
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Env) ->
    supervisor:start_child(?SERVER, [Env]).

init([]) ->
    Element = {aerl_en, {aerl_en, start_link, []},
               temporary, brutal_kill, worker, [aerl_en]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
