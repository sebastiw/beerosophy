%%%-------------------------------------------------------------------
%% @doc beerosophy public API
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy).

-export([ start/0
        , metrics/0
        , tick/1
        , python/1
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(beerosophy).

metrics() ->
    beerosophy_metrics:get_all().

tick(Metric) ->
    beerosophy_ticker:start(Metric).

python(Script) ->
    beerosophy_python:start(Script).
