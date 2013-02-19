#!/bin/sh
# TODO: Add error message if push_api.config is missing.
# Please copy priv/eredis_queue.config.example to priv/eredis_queue.config, customize it and restart server.
cd `dirname $0`
exec erl -sname eredis-queue +K true -pa $PWD/ebin $PWD/apps/*/ebin $PWD/deps/*/ebin -config $PWD/priv/eredis_queue.config -s eredis_queue_app
