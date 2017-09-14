%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beerosophy_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_WAIT, timer:seconds(4)).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    PrivDir = code:priv_dir(beerosophy),
    {ok, [Routes]} = file:consult(filename:join([PrivDir, "dispatch.conf"])),
    lager:info("Routes ~p", [Routes]),
    Dispatch = cowboy_router:compile(Routes),

    Port = application:get_env(beerosophy, port, ?DEFAULT_PORT),
    lager:info("Starting beerosophy at port ~B", [Port]),
    {ok, _} = cowboy:start_clear(beerosophy_http_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),

    timer:send_after(?DEFAULT_WAIT, start_scripts),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_scripts, State) ->
    PrivDir = code:priv_dir(beerosophy),
    {ok, [Pylons]} = file:consult(filename:join([PrivDir, "pylons.conf"])),
    lists:map(fun (#{name := N} = P) ->
                      lager:info("Starting python script '~s'", [N]),
                      {ok, _} = beerosophy:python(P)
              end, Pylons),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
