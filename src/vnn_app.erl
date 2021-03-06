%%--------------------------------------------------------------------------------------------------
%% @doc
%% Application main
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_app).

-behaviour (application).
-export ([start/2, stop/1]).

-include_lib ("lager/include/lager.hrl").

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start application
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start (normal | {takeover, node ()} | {failover, node ()}, term ()) -> {ok, pid ()} | {error, term ()}.
%%--------------------------------------------------------------------------------------------------
start (_StartType, _StartArgs) ->
    case vnn_sup:start_link () of
        {ok, Pid} ->
            ok = vnn_yaws_sup:start_children (),
            ok = vnn_event_ws_notifier:add_handler (),
            {ok, Pid};
        Other ->
            {error, Other}
    end.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stop application
%% @end
%%--------------------------------------------------------------------------------------------------
-spec stop (term ()) -> ok.
%%--------------------------------------------------------------------------------------------------
stop (_State) ->
    ok.
