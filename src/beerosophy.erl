%%%-------------------------------------------------------------------
%% @doc beerosophy public API
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy).

-export([ start/0
        , install/1
        , metrics/0
        , python/1
        , store/2
        , read_latest/1
        , read_day/2
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(beerosophy).

install(Nodes) ->
    beerosophy_database:install(Nodes).

metrics() ->
    beerosophy_metrics:get_all().

python(Script) ->
    beerosophy_python:start(Script).

store(Sensor, Data) ->
    beerosophy_database:store(Sensor, Data).

read_latest(Sensor) when is_binary(Sensor) ->
    read_latest(binary_to_atom(Sensor, utf8));
read_latest(Sensor) ->
    beerosophy_database:read_latest(Sensor).

read_day(Sensor, Day) when is_binary(Sensor) ->
    read_day(binary_to_atom(Sensor, utf8), Day);
read_day(Sensor, Day) when is_binary(Day) ->
    [Y, M, D] = binary:split(Day, <<"-">>, [global]),
    Date = {binary_to_integer(Y),
            binary_to_integer(M),
            binary_to_integer(D)},
    read_day(Sensor, Date);
read_day(Sensor, Day) ->
    beerosophy_database:select_day(Sensor, Day).
