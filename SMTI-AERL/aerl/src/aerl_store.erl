-module(aerl_store).
-include("aerl.hrl").

-export([init/0]).

-define(TIMEOUT, 2000).

init() ->
    aerl_connect:start_link(),
    aerl_connect:add_local_service(aerl, node()),
    aerl_connect:add_target_type(aerl),
    aerl_connect:trade_service(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    AErlNodes = find_nodes(),
    dynamic_db_init(lists:delete(node(), AErlNodes)).

find_nodes() ->
    aerl_connect:fetch_services().

dynamic_db_init([]) ->
    mnesia:create_table(aerl_store,
			[{attributes,record_info(fields,aerl_store)},{index,[#aerl_store.id]}]);
dynamic_db_init(Nodes) ->
    add_extra_nodes(Nodes).

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            mnesia:add_table_copy(schema, node(), ram_copies),
            mnesia:add_table_copy(aerl_store, node(), ram_copies),
            Tables = mnesia:system_info(tables),
            mnesia:wait_for_tables(Tables, ?TIMEOUT);
        _ ->
            add_extra_nodes(T)
    end.
