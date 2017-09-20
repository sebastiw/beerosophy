-module(beerosophy_metrics).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , inc/1
        , get_all/0
        ]).

%% Cowboy URIs
-export([ status/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Cowboy API
-export([ init/2
        , terminate/3
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {counters = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

inc(Metric) ->
    gen_server:call(?MODULE, {inc, Metric}).

get_all() ->
    gen_server:call(?MODULE, get_all).

%%====================================================================
%% Cowboy URIs
%%====================================================================

status(Req, State) ->
    Values = get_all(),
    Res = jsone:encode(Values),
    {Res, Req, State}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({inc, Metric}, _From, #state{counters = Ms} = State) ->
    Val = case Ms of
              #{Metric := Value} ->
                  Value;
              _ ->
                  0
          end,
    {reply, Val, State#state{counters = Ms#{Metric => Val+1}}};
handle_call(get_all, _From, State) ->
    {reply, State#state.counters, State};
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

%%====================================================================
%% Cowboy API
%%====================================================================

-spec init(Req, _Opts) ->
                  {ok, Req, State} |
                  {module(), Req, State} |
                  {module(), Req, State, hibernate | Timeout} |
                  {module(), Req, State, Timeout, hibernate}.

init(#{method := Method, path := Path} = Req, Opts) ->
    lager:info("~p: Incoming ~p request for ~p~n",
               [?MODULE, binary_to_list(Method), binary_to_list(Path)]),
    {cowboy_rest, Req, #{options => Opts}}.

-spec terminate(_Reason, _Req, _State) -> ok.

terminate(_Reason, _Req, _State) ->
  ok.

allowed_methods(Req, State) ->
    Methods = [ <<"GET">>
              , <<"HEAD">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    Types = [],
    {Types, Req, State}.

content_types_provided(Req, State) ->
    Types = [ {<<"application/json">>, status}
            ],
    {Types, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
