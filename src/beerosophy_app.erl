%%%-------------------------------------------------------------------
%% @doc beerosophy public API
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy_app).

-behaviour(application).

%% Application callbacks
-export([ start/2
        , start_phase/3
        , stop/1
        ]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _Args) ->
    beerosophy:install([node()]),
    application:start(mnesia),
    beerosophy_sup:start_link().

start_phase(start_scripts, normal, _Args) ->
    lager:info("Starting python scripts", []),
    PrivDir = code:priv_dir(beerosophy),
    {ok, [Pylons]} = file:consult(filename:join([PrivDir, "pylons.conf"])),
    lists:foreach(fun (P) -> {ok, _} = beerosophy:python(P) end,
                  Pylons).

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
