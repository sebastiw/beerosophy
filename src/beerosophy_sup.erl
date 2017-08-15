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
    {ok, {{one_for_all, 5, 10}, [child(beerosophy_server)]}}.

%%====================================================================
%% Internal functions
%%====================================================================

child(Mod) ->
    Restart = permanent,
    Shutdown = 5000,
    Type = worker,
    {Mod, {Mod, start_link, []}, Restart, Shutdown, Type, [Mod]}.
