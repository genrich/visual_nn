%%--------------------------------------------------------------------------------------------------
%% @doc
%% Global params shared with WebGL client
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_params).

-export ([spike_speed/0, set_spike_speed/1]).

-on_load (init/0).

-include_lib ("lager/include/lager.hrl").

-define (APPNAME, visual_nn).

-include ("const.hrl").

%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Load NIF and initialize common params from "const.hrl"
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init () -> ok.
%%--------------------------------------------------------------------------------------------------
init () ->
    case code:priv_dir (?APPNAME) of
        {error, _} -> 
            lager:error ("~w priv dir not found", [?APPNAME]),
            exit (error);
        PrivDir ->
            erlang:load_nif (filename:join ([PrivDir, ?MODULE]), 0)
    end,
    set_spike_speed (?SPIKE_SPEED),
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get spike propagation speed
%% @end
%%--------------------------------------------------------------------------------------------------
-spec spike_speed () -> float ().
%%--------------------------------------------------------------------------------------------------
spike_speed () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set spike propagation speed
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_spike_speed (Speed :: number ()) -> float ().
%%--------------------------------------------------------------------------------------------------
set_spike_speed (_Speed) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
spike_speed_test () ->
    ?assert (vnn_params:spike_speed () > 0).

set_spike_speed_exeption_test () ->
    ?assertError(badarg, vnn_params:set_spike_speed (atom)).

set_spike_speed_test () ->
    ?assert (vnn_params:set_spike_speed (99.0) =:= 99.0).

set_spike_speed_int_test () ->
    ?assert (vnn_params:set_spike_speed (99) =:= 99.0).

-endif.
