%% @author Mickael Rémond <mremond@process-one.net>
%% @copyright Copyright 2002-2013, ProcessOne

%% @doc Wrapper for the Redis Job Queue Application

-module(eredis_queue_app).
-author('Mickael Remond <mremond@process-one.net>').

-export([start/0]).

-behaviour(application).
-export([start/2,stop/1]).

%% Internal export
-export([launch_queues_in_first_pool/0]).

start() ->
    ensure_started(lager),
    ensure_started(eredis_pool),
    application:start(eredis_queue).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eredis_queue.
start(_Type, _StartArgs) ->
    {ok, Pid} = eredis_queue_sup:start_link(),
    %% spawn process in charge of starting all the APNS connections
    spawn(fun launch_queues_in_first_pool/0),
    {ok, Pid}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eredis_queue.
stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

launch_queues_in_first_pool() ->
    {PoolName, BinQueues} = eredis_queue:get_queue(),
    lists:foreach(
      fun(BinQueue) ->
	      eredis_queue_sup:start_child(BinQueue, PoolName)
      end, BinQueues).
