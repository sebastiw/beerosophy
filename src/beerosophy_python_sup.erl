-module(beerosophy_python_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_child/1
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one
                , intensity => 10
                , period => 5
                },
    {ok, {SupFlags, [ child(beerosophy_python) ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child(Mod) ->
    #{ id => Mod
     , start => {Mod, start_link, []}
     , restart => permanent
     , shutdown => brutal_kill
     , type => worker
     , modules => [Mod]
     }.
