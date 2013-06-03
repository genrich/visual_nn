-module (vnn_yaws_ws).

-export ([init/1, handle_open/2, handle_message/1, terminate/2]).

-include_lib ("lager/include/lager.hrl").

-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
-endif.

-define (STOP_SIMULATION,  0).
-define (START_SIMULATION, 1).

-record (state, {}).

init([_Arg, Params]) ->
    lager:debug ("ws init ~p: ~p~n", [self(), Params]),
    {ok, #state{}}.

handle_open (WsState, State) ->
    lager:debug ("ws handle open WsState=~p", [WsState]),
    vnn_event_ws_notifier:set_ws (WsState),
    {ok, State}.

handle_message ({binary, <<?STOP_SIMULATION:32/little-signed-integer>>}) ->
    vnn_network:sim_stop (),
    noreply;

handle_message ({binary, <<?START_SIMULATION:32/little-signed-integer>>}) -> 
    vnn_network:sim_start (),
    noreply;

handle_message ({Type, Data}) ->
    lager:error ("type=~p data=~p", [Type, Data]),
    noreply;

handle_message ({close, _CloseStatus, _Data}) ->
    lager:debug ("ws close"),
    vnn_network:sim_stop (),
    vnn_event_ws_notifier:set_ws (undefined),
    noreply.

terminate (_Reason, _State) ->
    vnn_event_ws_notifier:set_ws (undefined).
