%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stimulus
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_stimulus).

-export ([hello_world/0, next_spike/1]).

-include_lib ("lager/include/lager.hrl").

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Create stimulus array
%% @end
%%--------------------------------------------------------------------------------------------------
-spec hello_world () -> {nonempty_string (), pos_integer (), pos_integer ()}.
%%--------------------------------------------------------------------------------------------------
hello_world () ->
    Stimulus = "                                        "
               "                                        "
               "    X   X  XXXXX  X      X       XXX    "
               "    X   X  X      X      X      X   X   "
               "    X   X  X      X      X      X   X   "
               "    XXXXX  XXX    X      X      X   X   "
               "    X   X  X      X      X      X   X   "
               "    X   X  X      X      X      X   X   "
               "    X   X  XXXXX  XXXXX  XXXXX   XXX    "
               "                                        "
               "    X   X   XXX   XXXX   X      XXX     "
               "    X   X  X   X  X   X  X      X  X    "
               "    X   X  X   X  X   X  X      X   X   "
               "    X X X  X   X  XXXX   X      X   X   "
               "    X X X  X   X  X X    X      X   X   "
               "    XX XX  X   X  X  X   X      X  X    "
               "    X   X   XXX   X   X  XXXXX  XXX     "
               "                                        "
               "                                        "
               "                                        ",
    Stride = 40,
    Lines  = 20,
    {Stimulus, Stride, Lines}.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Return next spike time
%% @end
%%--------------------------------------------------------------------------------------------------
-spec next_spike (stimulus_active | stimulus_rest) -> float ().
%%--------------------------------------------------------------------------------------------------
next_spike (stimulus_active) ->
    vnn_random:exponential (vnn_params:active_rate ());

next_spike (stimulus_rest) ->
    vnn_random:exponential (vnn_params:rest_rate ()).
