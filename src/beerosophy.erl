%%%-------------------------------------------------------------------
%% @doc beerosophy public API
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy).

-export([ start/0
        , metrics/0
        , python/1
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(beerosophy).

metrics() ->
    beerosophy_metrics:get_all().

python(Script) ->
    beerosophy_python:start(Script).
