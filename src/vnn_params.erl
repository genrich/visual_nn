%%--------------------------------------------------------------------------------------------------
%% @doc
%% Global params shared with WebGL client
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_params).

-export ([now/0,
          slowdown/0,
          spike_speed/0,
          active_rate/0,
          rest_rate/0,
          absolute_refractory/0,
          set_slowdown/1,
          set_spike_speed/1,
          set_active_rate/1,
          set_rest_rate/1,
          set_absolute_refractory/1
         ]).

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
    set_slowdown (?PARAM_SLOWDOWN),
    set_spike_speed (?PARAM_SPIKE_SPEED),
    set_active_rate (?PARAM_ACTIVE_RATE),
    set_rest_rate (?PARAM_REST_RATE),
    set_absolute_refractory (?PARAM_ABSOLUTE_REFRACTORY),
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Current nano time
%% @end
%%--------------------------------------------------------------------------------------------------
-spec now () -> float ().
%%--------------------------------------------------------------------------------------------------
now () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get slowdown
%% @end
%%--------------------------------------------------------------------------------------------------
-spec slowdown () -> float ().
%%--------------------------------------------------------------------------------------------------
slowdown () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set slowdown
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_slowdown (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_slowdown (_) ->
    erlang:nif_error (nif_not_loaded).


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
-spec set_spike_speed (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_spike_speed (_) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get stimulus spike rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec active_rate () -> float ().
%%--------------------------------------------------------------------------------------------------
active_rate () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set stimulus spike rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_active_rate (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_active_rate (_) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get stimulus noise rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec rest_rate () -> float ().
%%--------------------------------------------------------------------------------------------------
rest_rate () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set stimulus noise rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_rest_rate (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_rest_rate (_) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get neuron absolute refractory time
%% @end
%%--------------------------------------------------------------------------------------------------
-spec absolute_refractory () -> float ().
%%--------------------------------------------------------------------------------------------------
absolute_refractory () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set spike propagation speed
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_absolute_refractory (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_absolute_refractory (_Speed) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
spike_speed_test () ->
    ?assert (vnn_params:spike_speed () > 0).

slowdown_test () ->
    ?assert (vnn_params:slowdown () > 0).

set_spike_speed_exeption_test () ->
    ?assertError(badarg, vnn_params:set_slowdown (atom)).

set_spike_speed_int_test () ->
    ?assertError (badarg, vnn_params:set_slowdown (99)).

set_spike_speed_test () ->
    ?assert (vnn_params:set_slowdown (99.0) =:= ok),
    ?assert (vnn_params:slowdown () =:= 99.0).

now_test () ->
    ?assert (is_float (vnn_params:now ())).

-endif.
