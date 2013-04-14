-module (vnn_yaws_ws).

-export ([handle_message/1]).

-include_lib ("lager/include/lager.hrl").

-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
-endif.

-define (STOP_SIMULATION,  0).
-define (START_SIMULATION, 1).

handle_message ({Type, Data}) ->
    case Type of
        binary ->
            lager:debug ("binary data ~p", [Data]),
            process_msg (Data),
            noreply
    end;

handle_message ({close, _CloseStatus, _Data}) ->
    lager:debug ("ws close"),
    vnn_network:sim_stop (),
    noreply.

process_msg (<<?STOP_SIMULATION:32/little-signed-integer>>)  -> vnn_network:sim_stop  ();
process_msg (<<?START_SIMULATION:32/little-signed-integer>>) -> vnn_network:sim_start ().
