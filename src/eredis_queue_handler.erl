-module(eredis_queue_handler).

-export([run/3]).

run(SQueue, BClass, Args) when is_list(SQueue),
			       is_binary(BClass) ->
    lager:info("[~s] Job received (~s): ~p", [SQueue, BClass, Args]);
run(Queue, Class, Args) ->
    lager:error("Incorrect parameters (~p, ~p, ~p)", [Queue, Class, Args]).
