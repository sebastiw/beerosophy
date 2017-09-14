-module(beerosophy_python).

-behaviour(gen_server).

%% API
-export([ start_link/1
        , start/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{name := Name} = Script) ->
    gen_server:start_link({local, Name}, ?MODULE, Script, []).

start(Script) ->
    supervisor:start_child(beerosophy_python_sup, [Script]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{command := Command, name := SName} = Script) ->
    process_flag(trap_exit, true),
    Options = maps:get(options, Script, []),
    Ports = open_port({spawn, Command}, Options),

    %% Start tickers for metrics
    Exposes = maps:get(exposes, Script, []),
    lists:map(fun (#{url := Url, metric := MName} = Metric) ->
                      lager:info("Starting ticker for '~s' on ~p",
                                 [MName, Url]),
                      beerosophy:tick(Metric#{script => SName,
                                              metric => MName});

                  (#{url := Url} = Metric) ->
                      lager:info("Starting ticker on ~p", [Url]),
                      beerosophy:tick(Metric#{script => SName})
              end,
              Exposes),

    {ok, Script#{port => Ports}}.

handle_call(Msg, _From, State) ->
    lager:debug("Got unknown call ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:debug("Got unknown cast ~p", [Msg]),
    {noreply, State}.

handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    lager:debug("~p: Got unknown info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
