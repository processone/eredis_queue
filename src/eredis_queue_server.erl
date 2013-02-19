%%%-------------------------------------------------------------------
%%% @author Mickaël Rémond <mremond@process-one.net>
%%% @copyright (C) 2013, ProcessOne
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2013 by Mickaël Rémond <mremond@process-one.net>
%%%-------------------------------------------------------------------
-module(eredis_queue_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([get_redis_pool/1, name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal loop export
-export([blpop_loop/2]).

-define(SERVER, ?MODULE). 

-record(state, {queue, redis_pool, loop_pid}).

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
start_link(Queue, RedisPool) when is_binary(Queue) ->
    start_link(binary_to_list(Queue), RedisPool);
start_link(Queue, RedisPool) when is_list(Queue), is_atom(RedisPool) ->
    gen_server:start_link({local, name(Queue)}, ?MODULE, [Queue, RedisPool], []).

get_redis_pool(Queue) when is_binary(Queue) ->
    get_redis_pool(binary_to_list(Queue));
get_redis_pool(Queue) when is_list(Queue) ->
    gen_server:call(name(Queue), get_redis_pool).

name(Queue) when is_atom(Queue) ->
    name(atom_to_list(Queue));
name(Queue) when is_binary(Queue) ->
    name(binary_to_list(Queue));
name(Queue) when is_list(Queue) ->
    list_to_atom("queue_" ++ Queue).

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
init([Queue, RedisPool]) ->
    Pid = spawn_link(?MODULE, blpop_loop, [self(), Queue]), 
    {ok, #state{queue=Queue, redis_pool=RedisPool, loop_pid=Pid}}.

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
handle_call(get_redis_pool, _From, #state{redis_pool= PoolName} = State) ->
    {reply, {ok, PoolName}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
blpop_loop(_Pid, _Queue) ->
    ok.
