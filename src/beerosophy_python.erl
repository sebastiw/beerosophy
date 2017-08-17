-module(beerosophy_python).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , start/1
        , message/1
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

-record(state, {pylons = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start({_, _} = P) ->
    gen_server:call(?MODULE, {start, P}).

message(F) ->
    gen_server:cast(?MODULE, {message, F()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    PrivDir = code:priv_dir(beerosophy),
    {ok, [Pylons]} = file:consult(filename:join([PrivDir, "pylons.conf"])),
    lager:info("Pylons ~p", [Pylons]),
    Ports = lists:map(fun ({Command, Opts}) ->
                              open_port({spawn, Command}, Opts)
                      end, Pylons),
    {ok, #state{pylons = Ports}}.

handle_call({start, {Command, Opts}}, _From, #state{pylons = Pys} = State) ->
    Port = open_port({spawn, Command}, Opts),
    lager:info("~p: started python process ~p (~p)", [?MODULE, Command, Port]),
    {reply, {ok, Port}, State#state{pylons = [Port|Pys]}}.

handle_cast(Msg, State) ->
    lager:warning("Got unknkown cast ~p", [Msg]),
    {noreply, State}.

handle_info({'EXIT', Port, Msg}, #state{pylons = Pys} = State) ->
    lager:warning("~p: Python process on port ~p died with reason ~p.",
                  [?MODULE, Port, Msg]),
    {noreply, State#state{pylons = [P || P <- Pys, P =/= Port]}};
handle_info(Info, State) ->
    lager:warning("~p: Got unknown info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{pylons = Pylons}) ->
    [python:stop(P) || P <- Pylons].

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
