-module(beerosophy_sensors).

-behaviour(cowboy_rest).

%% Cowboy URIs
-export([ today/2
        , latest/2
        , day/2
        ]).

%% Cowboy API
-export([ init/2
        , terminate/3
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        ]).

%%====================================================================
%% Cowboy URIs
%%====================================================================

today(Req, State) ->
    Sensor = cowboy_req:binding(sensor, Req),
    {Day, _} = erlang:localtime(),
    Val = case beerosophy:read_day(Sensor, Day) of
              {ok, Values} ->
                  Values;
              E ->
                  E
          end,
    Res = jsone:encode(Val),
    {binary_to_list(Res), Req, State}.

latest(Req, State) ->
    Sensor = cowboy_req:binding(sensor, Req),
    Val = case beerosophy:read_latest(Sensor) of
              {ok, Values} ->
                  Values;
              E ->
                  E
          end,
    Res = jsone:encode(Val),
    {Res, Req, State}.

day(Req, State) ->
    Sensor = cowboy_req:binding(sensor, Req),
    {DayDefault, _} = erlang:localtime(),
    Day = cowboy_req:binding(day, Req, DayDefault),
    Val = case beerosophy:read_day(Sensor, Day) of
              {ok, Values} ->
                  Values;
              E ->
                  E
          end,
    Res = jsone:encode(Val),
    {Res, Req, State}.

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
    Function = case State of
                   #{options := [X]} ->
                       X;
                   _ ->
                       day
               end,
    Types = [ {<<"application/json">>, Function}
            ],
    {Types, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
