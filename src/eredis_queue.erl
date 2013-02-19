%% Helper functions
-module(eredis_queue).

-export([get/0, get/1]).
-export([get_first_poolname/0]).

%% Returns queues defined in first configured redis pool:
get() ->
    case get_first_poolname() of
	{ok, PoolName} ->
	    case eredis_pool:q({global,PoolName}, ["SMEMBERS", "queues"]) of
		{ok, BinQueues} ->
		    {PoolName, BinQueues};
		_ ->
		    {PoolName, []}
	    end;
	not_found ->
	    {default, []}
    end.

get(PoolName) when is_atom(PoolName) ->
    case eredis_pool:q({global,PoolName}, ["SMEMBERS", "queues"]) of
	{ok, BinQueues} ->
	    {PoolName, BinQueues};
	_ ->
	    {PoolName, []}
    end.

get_first_poolname() ->
    case application:get_env(eredis_pool, pools) of
	{ok, [{PoolName, _Params}|_Pools]} ->
	    {ok, PoolName};
	_ ->
	    not_found
    end.
