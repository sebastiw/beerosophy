-module(beerosophy_ticker).

-behaviour(gen_server).

%% API
-export([ start_link/1
        , start/1
        , tick/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(INTERVAL_TIME, timer:seconds(10)).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Metric) ->
    ServerName = get_name(Metric),
    gen_server:start_link({local, ServerName}, ?MODULE, Metric, []).

start(Metric) ->
    supervisor:start_child(beerosophy_ticker_sup, [Metric]).

tick(Metric) ->
    erlang:whereis(get_name(Metric)) ! tick.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Metric) ->
    process_flag(trap_exit, true),
    {ok, _} = timer:send_interval(?INTERVAL_TIME, tick),
    {ok, Metric}.

handle_call(_Request, _From, Metric) ->
    {reply, ok, Metric}.

handle_cast(_Msg, Metric) ->
    {noreply, Metric}.

handle_info(tick, #{url := Url} = Metric) ->
    #{value := Value} = get_metric(Url),
    lager:info("~p: ~p", [Metric, Value]),
    {noreply, Metric};
handle_info(_Info, Metric) ->
    {noreply, Metric}.

terminate(_Reason, _Metric) ->
    ok.

code_change(_OldVsn, Metric, _Extra) ->
    {ok, Metric}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_name(#{script := Script, metric := Name}) ->
    list_to_atom(Script ++ "_" ++ Name);
get_name(#{url := Url}) ->
    list_to_atom(Url).

get_metric(Url) ->
    {ok, {{_V, 200, _R}, _H, Body}} = httpc:request(Url),
    jsone:decode(Body).

