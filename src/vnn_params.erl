%%--------------------------------------------------------------------------------------------------
%% @doc
%% Global params shared with WebGL client
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_params).

-export ([
          slowdown/0,
          spike_speed/0,
          spike_rate/0,
          noise_rate/0,
          absolute_refractory/0,
          set_slowdown/1,
          set_spike_speed/1,
          set_spike_rate/1,
          set_noise_rate/1,
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
    set_spike_rate (?PARAM_SPIKE_RATE),
    set_noise_rate (?PARAM_NOISE_RATE),
    set_absolute_refractory (?PARAM_ABSOLUTE_REFRACTORY),
    ok.


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
-spec spike_rate () -> float ().
%%--------------------------------------------------------------------------------------------------
spike_rate () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set stimulus spike rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_spike_rate (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_spike_rate (_) ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Get stimulus noise rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec noise_rate () -> float ().
%%--------------------------------------------------------------------------------------------------
noise_rate () ->
    erlang:nif_error (nif_not_loaded).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Set stimulus noise rate
%% @end
%%--------------------------------------------------------------------------------------------------
-spec set_noise_rate (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
set_noise_rate (_) ->
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

set_spike_speed_exeption_test () ->
    ?assertError(badarg, vnn_params:set_slowdown (atom)).

set_spike_speed_int_test () ->
    ?assertError (badarg, vnn_params:set_slowdown (99)).

set_spike_speed_test () ->
    ?assert (vnn_params:set_slowdown (99.0) =:= ok),
    ?assert (vnn_params:slowdown () =:= 99.0).

-endif.
