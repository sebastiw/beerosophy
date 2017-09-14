%%%-------------------------------------------------------------------
%% @doc beerosophy public API
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy).

-export([ start/0
        , python/1
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(beerosophy).

python(Script) ->
    beerosophy_python:start(Script).
