%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stimulus
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_stimulus).

-export ([create/0, next_spike/1]).

-include_lib ("lager/include/lager.hrl").

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Create stimulus array
%% @end
%%--------------------------------------------------------------------------------------------------
-spec create () -> [pid ()].
%%--------------------------------------------------------------------------------------------------
create () ->
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
    create (Stimulus, Stride, Lines, fun vnn_network:create_stimulus/5).


%%--------------------------------------------------------------------------------------------------
-spec create (String, Stride, Lines, Fun) -> [pid ()] when
      String :: nonempty_string (),
      Stride :: pos_integer (),
      Lines  :: pos_integer (),
      Fun    :: fun ((pos_integer (),
                      pos_integer (),
                      non_neg_integer (),
                      non_neg_integer (),
                      vnn_network:node_type (),
                      boolean ())
                     -> pid ()).
%%--------------------------------------------------------------------------------------------------
create (String, Stride, Lines, Fun) ->
    {Pids, _} = 
        lists:mapfoldl (fun ($X, {I0, Strd, Strd}) -> I = I0+1, J = 0, {Fun (Stride, Lines, I, J, stimulus_active),  {I, J+1, Strd}};
                            ($X, {I,  J,    Strd}) ->                  {Fun (Stride, Lines, I, J, stimulus_active),  {I, J+1, Strd}};
                            ($ , {I0, Strd, Strd}) -> I = I0+1, J = 0, {Fun (Stride, Lines, I, J, stimulus_rest),    {I, J+1, Strd}};
                            ($ , {I,  J,    Strd}) ->                  {Fun (Stride, Lines, I, J, stimulus_rest),    {I, J+1, Strd}} end,
                        {0, 0, Stride}, String),
    Pids.


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
