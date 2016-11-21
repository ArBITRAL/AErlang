%%%-------------------------------------------------------------------
%%% @author tan duong <tanduong@localhost>
%%% @copyright (C) 2016, tan duong
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2016 by tan duong <tanduong@localhost>
%%%-------------------------------------------------------------------
-module(psmp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Path = code:priv_dir(psmp),
    {ok, Size} = application:get_env(psmp,size),
    PATH = Path ++ "/size-" ++ integer_to_list(Size) ++ "/",
    {ok, DirList} = file:list_dir(PATH),
    {ok, N} = application:get_env(psmp,run),
    psmp:run(N,PATH,DirList),
    {ok, self()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
