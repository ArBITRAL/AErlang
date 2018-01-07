-module(aerl_connect).

-behaviour(gen_server).

%% API
-export([start_link/0,
	add_local_service/2,
	add_target_type/1,
	fetch_services/0,
	trade_service/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_type,local_service,found_services}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_type(Type) ->
    gen_server:cast(?SERVER, {add_target_type, Type}).

add_local_service(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_service, {Type, Instance}}).

%% Fetch service instances given a Type
fetch_services() ->
    gen_server:call(?SERVER, fetch_services).

trade_service() ->
    gen_server:cast(?SERVER, trade_service).

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
    {ok, #state{found_services = []}}.

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
handle_call(fetch_services, _From, State) ->
    {reply, State#state.found_services, State}.

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
handle_cast({add_target_type, Type}, State) ->
    {noreply, State#state{target_type = Type}};
handle_cast({add_local_service, {Type, Instance}}, State) ->
    {noreply, State#state{local_service = {Type,Instance}}};
handle_cast(trade_service, State) ->
    LocalService = State#state.local_service,
    AllNodes = [node() | nodes()],
     lists:foreach(
        fun(Node) ->
            gen_server:cast({?SERVER, Node},
                            {trade_service, {node(), LocalService}})
        end,
         AllNodes),
    {noreply, State};
handle_cast({trade_service, {ReplyTo, Remote}},
	    #state{local_service = Local,
		  target_type = TargetType,
		  found_services = OldFound} = State) ->
    Filtered = filter_service_type(TargetType, Remote),
    New = add_service(Filtered, OldFound),
    case ReplyTo of
	nopeply -> ok;
	_ -> gen_server:cast({?SERVER, ReplyTo}, {trade_service, {noreply, Local}})
    end,
    {noreply, State#state{found_services = New}}.

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

add_service([], OldFound) ->
    OldFound;
add_service([Instance], OldFound) ->
    case lists:member(Instance,OldFound) of
	true ->
	    OldFound;
	false -> [Instance | OldFound]
    end.
filter_service_type(Type, ServiceDesc) ->
    case ServiceDesc of
	{Type, Instance} -> [Instance];
	_ -> []
    end.
