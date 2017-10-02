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

-define(INTERVAL, timer:seconds(30)).

-record(state, {script, state = not_running, port}).

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

init(Script) ->
    process_flag(trap_exit, true),
    self() ! execute,
    timer:send_interval(?INTERVAL, execute),
    {ok, #state{script = Script}}.

handle_call(Msg, _From, State) ->
    lager:debug("Got unknown call ~p", [Msg]),
    {reply, error, State}.

handle_cast(Msg, State) ->
    lager:debug("Got unknown cast ~p", [Msg]),
    {noreply, State}.

%% Start of script
handle_info(execute, #state{state=not_running, script=Script} = State) ->
    #{name := Name, command := Command} = Script,
    beerosophy:inc(counter_name(Script, "runs")),
    Options = maps:get(options, Script, []),
    %% Run Python command
    lager:debug("Running python script '~s'", [Name]),
    Port = erlang:open_port({spawn, Command}, [exit_status|Options]),
    {noreply, State#state{state=running, port=Port}};
handle_info(execute, #state{state=running, script=Script, port=Port} = State) ->
    #{name := Name} = Script,
    beerosophy:inc(counter_name(Script, "long_running")),
    lager:warning("~p: Killing long running script ~p", [?MODULE, Name]),
    lager:warning("~p: ~p", [Name, erlang:port_info(Port)]),
    erlang:port_close(Port),
    {noreply, State};

%% Exits
handle_info({_Port, {exit_status, 0}}, State) ->
    #state{script=#{name := Name} = Script} = State,
    beerosophy:inc(counter_name(Script, "terminated_successfully")),
    lager:debug("~p: Python script ~p terminated successfully",
                [?MODULE, Name]),
    {noreply, State#state{state=not_running, port=undefined}};
handle_info({_Port, {exit_status, Exit}}, State) ->
    #state{script=#{name := Name} = Script} = State,
    beerosophy:inc(counter_name(Script, "terminated_unsuccessfully")),
    lager:error("~p: Python script ~p died with exit status ~p",
                [?MODULE, Name, Exit]),
    {noreply, State#state{state=not_running, port=undefined}};
handle_info({'EXIT', _Port, normal}, State) ->
    #state{script=#{name := Name} = Script} = State,
    beerosophy:inc(counter_name(Script, "terminated_exit")),
    lager:debug("~p: Python script ~p stopped", [?MODULE, Name]),
    {noreply, State#state{state=not_running, port=undefined}};

%% Messages from script
handle_info({_Port, {data, {_, Stdout}}},
            #state{script=#{name := temperature=Name}} = State) ->
    lager:info("~p: ~p: ~p", [?MODULE, Name, Stdout]),
    Temp = hd(string:tokens(Stdout, " ")),
    {T, []} = string:to_float(Temp),
    %% Save to db
    beerosophy:store(Name, #{value => T}),
    {noreply, State};
handle_info({_Port, {data, {_, Stdout}}},
            #state{script=#{name := Name}} = State) ->
    lager:info("~p: ~p: ~p", [?MODULE, Name, Stdout]),
    %% Save to db
    beerosophy:store(Name, Stdout),
    {noreply, State};

%% Unhandled messages
handle_info(Info, State) ->
    lager:warning("~p: Got unknown info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

counter_name(#{name := Name}, Operation)  ->
    list_to_atom(atom_to_list(Name) ++ [$_|Operation]).
