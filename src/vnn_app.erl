-module (vnn_app).

-behaviour (application).
-export ([start/2, stop/1]).

-include_lib ("lager/include/lager.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start (_StartType, _StartArgs) ->
    case vnn_sup:start_link () of
        {ok, Pid} ->
            ok = vnn_yaws_sup:start_children (),
            ok = vnn_event_ws_notifier:add_handler (),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop (_State) ->
    ok.
