%%%-------------------------------------------------------------------
%% @doc beerosophy database handler
%% @end
%%%-------------------------------------------------------------------

-module(beerosophy_database).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([ start_link/0
        , install/1
        , store/2
        , select_day/2
        , read_latest/1
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
-record(state, {last_keys}).

-record(data, {key, value}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec install([node()]) -> {[ok], [node()]}.

install(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok
    end,
    ok = mnesia:start(),
    case mnesia:create_table(data,
                             [{attributes, record_info(fields, data)},
                              {disc_copies, Nodes},
                              {type, set}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, data}} ->
            ok
    end.

-spec store(atom(), term()) -> ok | error.

store(Sensor, Value) ->
    gen_server:cast(?MODULE, {store, Sensor, Value}).

-spec select_day(atom(), calendar:date()) -> {error, term()} | {ok, term()}.

select_day(Sensor, Day) ->
    gen_server:call(?MODULE, {select_day, Sensor, Day}).

-spec read_latest(atom()) -> {ok, term()} | error.

read_latest(Sensor) ->
    gen_server:call(?MODULE, {read_latest, Sensor}).

%%====================================================================
%% gen_server API
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    Keys = read_keys(),
    lager:info("~p: started", [?MODULE]),
    {ok, #state{last_keys=Keys}}.

handle_cast({store, Sensor, Value}, #state{last_keys=Keys} = State) ->
    lager:info("~p: store value in ~p", [?MODULE, Sensor]),
    Now = calendar:universal_time(),
    mnesia:dirty_write(#data{key={Sensor, Now}, value=Value}),
    {noreply, State#state{last_keys=Keys#{Sensor=>Now}}};
handle_cast(_What, State) ->
    {noreply, State}.

handle_call({select_day, Sensor, Day}, _Who, State) ->
    Match = ets:fun2ms(fun (#data{key={S, {D, _}}} = Data)
                             when S =:= Sensor, D =:= Day ->
                               Data
                       end),
    Reply = mnesia:transaction(
              fun () ->
                      V = mnesia:select(data, Match),
                      lists:sort(V)
              end),
    case Reply of
        {aborted, Reason} ->
            {reply, {error, {aborted, Reason}}, State};
        {atomic, Values} ->
            {reply, {ok, encode(Values)}, State}
    end;
handle_call({read_latest, Sensor}, _Who, #state{last_keys=Keys} = State) ->
    case Keys of
        #{Sensor := Time} ->
            {atomic, [Val]} = mnesia:transaction(
                                fun () ->
                                        mnesia:read({data, {Sensor, Time}})
                                end),
            {reply, {ok, encode(Val)}, State};
        _ ->
            {reply, sensor_not_found, State}
    end.

handle_info(_What, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Why, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_keys() ->
    {atomic, Keys} =
        mnesia:transaction(
          fun () ->
                  Keys = mnesia:all_keys(data),
                  lists:foldl(fun ({S, Date}, Acc) ->
                                     case Acc of
                                         #{S := ODate} when ODate >= Date ->
                                             Acc;
                                         _ ->
                                             Acc#{S => Date}
                                     end
                             end, #{}, Keys)
          end),
    Keys.

encode([]) ->
    [];
encode([D|Rest]) ->
    [encode(D)|encode(Rest)];
encode(#data{key={temperature, {{Y,Mo,D}, {H,Mi,S}}}, value=#{value := Val}}) ->
    Dt = io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                       [Y, Mo, D, H, Mi, S]),
    DateString = list_to_binary(lists:flatten(Dt)),
    #{sensor => temperature, datetime => DateString, value => erlang:float_to_binary(Val)};
encode(#data{key={Sensor, {{Y,Mo,D}, {H,Mi,S}}}, value=Val}) ->
    Dt = io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                       [Y, Mo, D, H, Mi, S]),
    DateString = list_to_binary(lists:flatten(Dt)),
    #{sensor => Sensor, datetime => DateString, value => list_to_binary(Val)}.
