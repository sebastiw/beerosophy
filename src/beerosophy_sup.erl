%%%-------------------------------------------------------------------
%% @doc beerosophy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{ strategy => one_for_one
                , intensity => 5
                , period => 10
                },
    {ok, {SupFlags, [ child(beerosophy_python_sup, supervisor)
                    , child(beerosophy_server, worker)
                    , child(beerosophy_metrics, worker)
                    , child(beerosophy_database, worker)
                    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

child(Mod, Type) ->
    #{ id => Mod
     , start => {Mod, start_link, []}
     , restart => permanent
     , shutdown => 5000
     , type => Type
     , modules => [Mod]
     }.
