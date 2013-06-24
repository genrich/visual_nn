%%--------------------------------------------------------------------
%% @doc
%% Web socket notifier event handler
%% @end
%%--------------------------------------------------------------------
-module (vnn_event_ws_notifier).

-export ([add_handler/0, delete_handler/0, set_ws/1]).

-behaviour (gen_event).
-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include_lib ("lager/include/lager.hrl").
-include_lib ("yaws/include/yaws_api.hrl").

-record (state, {ws :: #ws_state{}}).

-include ("const.hrl").

-define (SEGM_LENGTH, 10).

%%--------------------------------------------------------------------
%% @doc
%% Register event handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler () -> ok | {'EXIT', term ()} | term ().

add_handler () ->
    vnn_event:add_handler (?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Remove event handler
%% @end
%%--------------------------------------------------------------------
-spec delete_handler () -> term () | {error, module_not_found} | {'EXIT', term ()}.

delete_handler () ->
    vnn_event:delete_handler (?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Remember web socket ref
%% @end
%%--------------------------------------------------------------------
-spec set_ws (WSState :: undefined | #ws_state{}) -> ok.

set_ws (Ws) ->
    ok = gen_event:call (vnn_event, ?MODULE, {set_ws, Ws}).

%%--------------------------------------------------------------------
%% @doc
%% Initialize
%% @end
%%--------------------------------------------------------------------
-spec init ([]) -> {ok, #state{}}.

init ([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Process synchronous calls
%% @end
%%--------------------------------------------------------------------
-spec handle_call ({set_ws, #ws_state{}}, #state{}) -> {ok, term (), #state{}}.

handle_call({set_ws, Ws}, _State) ->
    Reply = ok,
    lager:debug ("setting ws = ~p", [Ws]),
    {ok, Reply, #state{ws = Ws}}.

%%--------------------------------------------------------------------
%% @doc
%% Handle events
%% @end
%%--------------------------------------------------------------------
-spec handle_event  ({send_stimulus_pos, Id, Pos}, #state{}) -> {ok, #state{}} when
                        Id :: non_neg_integer (),
                        Pos :: vnn_network:position ();
                    ({send_soma_pos, Id, Pos}, #state{}) -> {ok, #state{}} when
                        Id :: non_neg_integer (),
                        Pos :: vnn_network:position ();
                    ({send_connection, Id, FromPos, ToPos}, #state{}) -> {ok, #state{}} when
                        Id :: non_neg_integer (),
                        FromPos :: vnn_network:position (),
                        ToPos :: vnn_network:position ();
                    ({send_stimulus_spike, Id}, #state{}) -> {ok, #state{}} when
                        Id :: non_neg_integer ();
                    ({send_event, term ()}, #state{}) -> {ok, #state{}}.

handle_event (_, #state{ws = undefined} = State) ->
    {ok, State};

handle_event ({send_stimulus_pos, Id, {X, Y, Z}}, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?STIMULUS_POS:32/little, Id:32/little,
                                                               X:32/little-float, Y:32/little-float, Z:32/little-float>>}),
    {ok, State};

handle_event ({send_soma_pos, Id, {X, Y, Z}}, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?SOMA_POS:32/little, Id:32/little,
                                                           X:32/little-float, Y:32/little-float, Z:32/little-float>>}),
    {ok, State};

handle_event ({send_connection, ConnId, FromPos, ToPos}, #state{ws = Ws} = State) ->
    {SegmentCount, SegmentLength, Segments} = segmentate (FromPos, ToPos),
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?CONNECTION:32/little, ConnId:32/little,
                                                   SegmentCount:32/little, SegmentLength:32/little-float,
                                                   << <<X:32/little-float, Y:32/little-float, Z:32/little-float>>
                                                     || {X, Y, Z} <- Segments >>/binary >>}),
    {ok, State};

%% handle_event ({send_connection, _, _, _}, State) ->
%%     {ok, State};

handle_event ({send_stimulus_spike, StimulusId, Connections}, #state{ws = Ws} = State) ->
    {ConnCount, ConnBinary} = lists:foldl (fun (ConnId, {Count, Bin}) ->
                                               {Count + 1, <<ConnId:32/little, Bin/binary>>} end, {0, <<>>}, Connections),
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?STIMULUS_SPIKE:32/little, StimulusId:32/little, ConnCount:32/little,
                                                   ConnBinary/binary>>}),
    {ok, State};

handle_event (Event, _) ->
    throw ({unknown_event, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Handle other messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info (Info :: term (), #state{}) -> {ok, #state{}}.

handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handle event handle removal from event manager
%% @end
%%--------------------------------------------------------------------
-spec terminate (Reason :: term () | {stop, term ()} | stop | remove_handler | {error, {'EXIT', term ()}} | {error, term ()}, #state{}) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Handle release upgrade/downgrade
%% @end
%%--------------------------------------------------------------------
-spec code_change (OldVsn :: term () | {down, term ()}, #state{}, Extra :: term ()) -> {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

segmentate ({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    DeltaX = X2 - X1, DeltaY = Y2 - Y1, DeltaZ = Z2 - Z1,
    Length = math:sqrt (math:pow (DeltaX, 2) + math:pow (DeltaY, 2) + math:pow (DeltaZ, 2)),
    N = trunc (math:pow (2, trunc (log2 (Length / ?SEGM_LENGTH)))),
    {N * 2, Length / N, lists:flatten ([[{X1 + DeltaX * I / N,     Y1 + DeltaY * I / N,     Z1 + DeltaZ * I / N},
                                         {X1 + DeltaX * (I+1) / N, Y1 + DeltaY * (I+1) / N, Z1 + DeltaZ * (I+1) / N}]
                                        || I <- lists:seq (0, N-1)])}.

log2 (X) -> math:log (X) / math:log (2).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

segments_test () ->
    {PointsCount, SegmentLength, Segments} = segmentate ({0, 0, 0}, {20, 0, 0}),
    ?assertEqual (4, PointsCount),
    ?assertEqual (10.0, SegmentLength),
    ?assertEqual ([{0.0, 0.0, 0.0}, {10.0, 0.0, 0.0}, {10.0, 0.0, 0.0}, {20.0, 0.0, 0.0}], Segments),
    {A, _, _} = segmentate ({0, 0, 0}, {50, 0, 0}),
    ?assertEqual (8, A).

-endif.
