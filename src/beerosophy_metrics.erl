-module(beerosophy_metrics).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , add/1
        , get_all/0
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

-record(state, {metrics = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Metric) ->
    gen_server:call(?MODULE, {add, Metric}).

get_all() ->
    gen_server:call(?MODULE, get_all).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({add, Metric}, _From, #state{metrics = Ms} = State) ->
    {reply, ok, State#state{metrics = [Metric|Ms]}};
handle_call(get_all, _From, State) ->
    {reply, State#state.metrics, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
