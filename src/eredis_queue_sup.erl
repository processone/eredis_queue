%%%-------------------------------------------------------------------
%%% @author Mickael Remond <mremond@process-one.net>
%%% @copyright (C) 2002-2013, ProcessOne
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2013 by Mickael Remond <mrempond@process-one.net>
%%%-------------------------------------------------------------------
-module(eredis_queue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, restart_child/1, stop_child/1]).

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

start_child(Queue, RedisPool) ->
    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,
    ChildSpec = {eredis_queue_server:name(Queue), {eredis_queue_server, start_link, [Queue, RedisPool]},
                 Restart, Shutdown, Type, [eredis_queue_server]},
    supervisor:start_child(?SERVER, ChildSpec).

stop_child(Queue) ->
    supervisor:terminate_child(?SERVER, eredis_queue_server:name(Queue)),
    supervisor:delete_child(?SERVER, eredis_queue_server:name(Queue)).

restart_child(Queue) ->
    {ok, PoolName} = eredis_server:get_poolname(eredis_queue_server:name(Queue)),
    stop_child(Queue),
    start_child(Queue, PoolName).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

