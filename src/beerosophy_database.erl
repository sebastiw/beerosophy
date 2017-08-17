%%%-------------------------------------------------------------------
%% @doc beerosophy database handler
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy_database).

-behaviour(cowboy_rest).

-behaviour(gen_server).

%% API
-export([ start_link/0
        ]).

%% Cowboy URIs
-export([ save_to_db/2
        , read_from_db/2
        ]).

%% Cowboy API
-export([ init/2
        , terminate/3
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        ]).

%% gen_server API
-export([ init/1
        , handle_cast/2
        , handle_call/3
        , handle_info/2
        , code_change/3
        , terminate/2
        ]).

%% gen_server defines & records
-record(state, {}).

-record(data, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec save_to_db({string(), iolist()}) -> ok | error.

save_to_db({Key, Data}) ->
    gen_server:call(?MODULE, {save, Key, Data}).

-spec read_from_db(string()) -> {ok, iolist()} | error.

read_from_db(Key) ->
    gen_server:call(?MODULE, {read, Key}).

%%====================================================================
%% Cowboy URIs
%%====================================================================

save_to_db(Req, State) ->
    Key = "Get from URI",
    Data = "Get from cowboy_req",
    Res = save_to_db({Key, Data}),
    {Res, Req, State}.

read_from_db(Req, State) ->
    Key = "Get from URI",
    Res = read_from_db(Key),
    {Res, Req, State}.

%%====================================================================
%% Cowboy API
%%====================================================================

-spec init(Req, _Opts) ->
                  {ok, Req, State} |
                  {module(), Req, State} |
                  {module(), Req, State, hibernate | Timeout} |
                  {module(), Req, State, Timeout, hibernate}.

init(#{method := Method, path := Path} = Req, _Opts) ->
    lager:info("~p: Incoming ~p request for ~p",
               [?MODULE, binary_to_list(Method), binary_to_list(Path)]),
    {cowboy_rest, Req, #{}}.

-spec terminate(_Reason, _Req, _State) -> ok.

terminate(_Reason, _Req, _State) ->
  ok.

allowed_methods(Req, State) ->
    Methods = [ <<"GET">>
              , <<"HEAD">>
              , <<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    Types = [ {<<"text/plain">>, save_to_db}
            , {<<"application/json">>, save_to_db}
            , {<<"application/x-www-form-urlencoded">>, save_to_db}
            ],
    {Types, Req, State}.

content_types_provided(Req, State) ->
    Types = [ {<<"application/json">>, read_from_db}
            , {<<"text/plain">>, read_from_db}
            ],
    {Types, Req, State}.

%%====================================================================
%% gen_server API
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(data, [{attributes, record_info(fields, data)}]),

    lager:info("~p: started", [?MODULE]),
    {ok, #state{}}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_call({read, K}, _Who, State) ->
    lager:info("~p: read ~p", [?MODULE, K]),
    {reply, read(K), State};
handle_call({store, K, D}, _Who, State) ->
    lager:info("~p: store ~p", [?MODULE, K]),
    {reply, store(K, D), State}.

handle_info(_What, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Why, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

read(_K) ->
    {ok, ""}.

store(_K, _D) ->
    ok.
