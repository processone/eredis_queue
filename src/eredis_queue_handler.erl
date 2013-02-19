-module(eredis_queue_handler).

-export([run/3]).

run(Queue, Class, Args) when is_list(Queue),
			     is_list(Class),
			     is_list(Args) ->
    lager:info("[~s] Job received (~s): ~p", [Queue, Class, Args]);
run(Queue, Class, Args) ->
    lager:error("Incorrect parameters (~p, ~p, ~p)", [Queue, Class, Args]).
