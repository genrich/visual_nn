%%--------------------------------------------------------------------
%% @doc
%% Yaws websocket callback
%% @end
%%--------------------------------------------------------------------
-module (vnn_yaws_ws).

-export ([init/1, handle_open/2, handle_message/1, terminate/2]).

-include_lib ("lager/include/lager.hrl").
-include_lib ("yaws/include/yaws_api.hrl").

-include ("const.hrl").

-record (state, {}).

%%--------------------------------------------------------------------
%% @doc
%% Initialize callback module
%% @end
%%--------------------------------------------------------------------
-spec init (ArgsListOfTwo) -> {ok, #state{}} when
      ArgsListOfTwo :: [ReqArg | InitialState],
      ReqArg        :: #arg{},
      InitialState  :: term ().

init([_Arg, Params]) ->
    lager:debug ("ws init ~p: ~p~n", [self(), Params]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handle connection upgrade from HTTP to WebSockets
%% @end
%%--------------------------------------------------------------------
-spec handle_open (WSState, #state{}) -> {ok, #state{}} when
      WSState :: #ws_state{}.

handle_open (WsState, State) ->
    lager:debug ("ws handle open WsState=~p", [WsState]),
    vnn_event_ws_notifier:set_ws (WsState),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handle websocket inbound messages
%% @end
%%--------------------------------------------------------------------
-spec handle_message ({binary, binary ()}) -> noreply;
                     ({text,   binary ()}) -> noreply;
                     ({close, RFC6455StatusCode, Reason}) -> noreply when RFC6455StatusCode :: integer (), Reason :: binary ().

handle_message ({binary, <<?STOP_SIMULATION:32/little-signed-integer>>}) ->
    vnn_network:sim_stop (),
    noreply;

handle_message ({binary, <<?START_SIMULATION:32/little-signed-integer>>}) ->
    vnn_network:sim_start (),
    noreply;

handle_message ({Type, Data}) ->
    lager:error ("type=~p data=~p", [Type, Data]),
    noreply;

handle_message ({close, _, _}) ->
    lager:debug ("ws close"),
    vnn_network:sim_stop (),
    vnn_event_ws_notifier:set_ws (undefined),
    noreply.

%%--------------------------------------------------------------------
%% @doc
%% Handle websocket process termination
%% @end
%%--------------------------------------------------------------------
-spec terminate (RFC6455StatusCode | {RFC6455StatusCode, Text} | {error, Error}, #state{}) -> ok when
    RFC6455StatusCode :: integer (),
    Text              :: binary (),
    Error             :: term ().

terminate (_Reason, _State) ->
    ok = vnn_event_ws_notifier:set_ws (undefined).
