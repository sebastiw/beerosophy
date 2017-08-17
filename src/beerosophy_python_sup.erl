-module(beerosophy_python_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0
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

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{ strategy => one_for_one
                , intensity => 1
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
     , shutdown => 5000
     , type => worker
     , modules => [Mod]
     }.
