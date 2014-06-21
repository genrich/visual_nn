%%--------------------------------------------------------------------------------------------------
%% @doc
%% Web socket notifier event handler
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_event_ws_notifier).

-export ([add_handler/0, delete_handler/0, set_ws/1]).

-behaviour (gen_event).
-export ([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include_lib ("lager/include/lager.hrl").
-include_lib ("yaws/include/yaws_api.hrl").

-record (state, {ws :: #ws_state{}}).

-include ("const.hrl").

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Register event handler
%% @end
%%--------------------------------------------------------------------------------------------------
-spec add_handler () -> ok | {'EXIT', term ()} | term ().
%%--------------------------------------------------------------------------------------------------
add_handler () ->
    vnn_event:add_handler (?MODULE, []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Remove event handler
%% @end
%%--------------------------------------------------------------------------------------------------
-spec delete_handler () -> term () | {error, module_not_found} | {'EXIT', term ()}.
%%--------------------------------------------------------------------------------------------------
delete_handler () ->
    vnn_event:delete_handler (?MODULE, []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Remember web socket ref
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_ws (WSState :: undefined | #ws_state{}) -> ok.
%%--------------------------------------------------------------------------------------------------
set_ws (Ws) ->
    ok = gen_event:call (vnn_event, ?MODULE, {set_ws, Ws}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Initialize
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init ([]) -> {ok, #state{}}.
%%--------------------------------------------------------------------------------------------------
init ([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Process synchronous calls
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_call ({set_ws, #ws_state{}}, #state{}) -> {ok, term (), #state{}}.
%%--------------------------------------------------------------------------------------------------
handle_call({set_ws, Ws}, _State) ->
    Reply = ok,
    lager:debug ("setting ws = ~p", [Ws]),
    {ok, Reply, #state{ws = Ws}}.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Handle events
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_event  ({notify_position, Id, Position}, #state{}) -> {ok, #state{}} when
                        Id       :: non_neg_integer (),
                        Position :: vnn_network:position ();
                    ({notify_spike, Id}, #state{}) -> {ok, #state{}} when
                        Id :: non_neg_integer ();
                    ({notify_connection, FromId, ToId}, #state{}) -> {ok, #state{}} when
                        FromId :: non_neg_integer (),
                        ToId   :: non_neg_integer ();
                    ({send_event, term ()}, #state{}) -> {ok, #state{}}.
%%--------------------------------------------------------------------------------------------------
handle_event (_, #state{ws = undefined} = State) ->
    {ok, State};

handle_event ({notify_position, Id, {X, Y, Z}}, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?POSITION:32/little, Id:32/little,
                                                   X:32/little-float, Y:32/little-float, Z:32/little-float>>}),
    {ok, State};

handle_event ({notify_spike, Id}, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?SPIKE:32/little, Id:32/little>>}),
    {ok, State};

handle_event ({notify_connection, FromId, ToId}, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?CONNECTION:32/little, FromId:32/little, ToId:32/little>>}),
    {ok, State};

handle_event (notify_new_network, #state{ws = Ws} = State) ->
    [ok] = yaws_api:websocket_send (Ws, {binary, <<?NEW_NETWORK:32/little>>}),
    {ok, State};

handle_event (Event, _) ->
    throw ({unknown_event, Event}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Handle other messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_info (Info :: term (), #state{}) -> {ok, #state{}}.
%%--------------------------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Handle event handle removal from event manager
%% @end
%%--------------------------------------------------------------------------------------------------
-spec terminate (Reason :: term () | {stop, term ()} | stop | remove_handler | {error, {'EXIT', term ()}} | {error, term ()}, #state{}) -> ok.
%%--------------------------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Handle release upgrade/downgrade
%% @end
%%--------------------------------------------------------------------------------------------------
-spec code_change (OldVsn :: term () | {down, term ()}, #state{}, Extra :: term ()) -> {ok, #state{}}.
%%--------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
