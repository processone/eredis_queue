%% Helper functions
-module(eredis_queue).

-export([get_queue/0, get_queue/1]).
-export([get_first_poolname/0]).

%% Returns queues defined in first configured redis pool:
get_queue() ->
    case get_first_poolname() of
	{ok, PoolName} ->
	    get_queue(PoolName);
	not_found ->
	    {default, []}
    end.

get_queue(PoolName) when is_atom(PoolName) ->
    case eredis_pool:q({global,PoolName}, ["SMEMBERS", "queues"]) of
	{ok, undefined} ->
	    create_default_queue(PoolName);
	{ok, []} ->
	    create_default_queue(PoolName);
	{ok, BinQueues} ->
	    {PoolName, BinQueues};
	_ ->
	    create_default_queue(PoolName)
    end.

get_first_poolname() ->
    case application:get_env(eredis_pool, pools) of
	{ok, [{PoolName, _Params}|_Pools]} ->
	    {ok, PoolName};
	_ ->
	    not_found
    end.

create_default_queue(PoolName) ->
    case get_default_queue() of
	undefined ->
	    {PoolName, []};
	Queue ->
	    case eredis_pool:q({global,PoolName}, ["SADD", "queues",Queue]) of
		{ok, _} ->
		    {PoolName, [list_to_binary(Queue)]};
		_ ->
		    {PoolName, []}
	    end
    end.

get_default_queue() ->
    case application:get_env(eredis_queue, default_queue) of
	{ok, Queue} when is_list(Queue) -> Queue;
	{ok, Queue} when is_atom(Queue) -> atom_to_list(Queue);
	undefined -> undefined
    end.
