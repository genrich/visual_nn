%%--------------------------------------------------------------------
%% @doc
%% Stimulus
%% @end
%%--------------------------------------------------------------------
-module (vnn_stimulus).

-export ([start/0, loop/1]).

-include_lib ("lager/include/lager.hrl").

-record (state, {absolute_refractory = 1 :: pos_integer (),
                 active                  :: boolean (),
                 id                      :: non_neg_integer (),
                 network                 :: pid (),
                 noise_rate = 0.01       :: float (),
                 position                :: vnn_network:position (),
                 spike_rate = 0.5        :: float (),
                 timer_ref               :: reference ()}).

-define (STRIDE, 40).
-define (LINES,  20).
-define (STEP,   20).

%%--------------------------------------------------------------------
%% @doc
%% Start stimulus array
%% @end
%%--------------------------------------------------------------------
-spec start () -> [pid ()].

start () ->
    X =
        "                                        "
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
    {Pids, _} =
        lists:mapfoldl (fun ($X, {I0, ?STRIDE, ?STRIDE}) -> I = I0+1, J = 0, {start (I, J, true),  {I, J+1, ?STRIDE}};
                            ($X, {I,        J, ?STRIDE}) ->                  {start (I, J, true),  {I, J+1, ?STRIDE}};
                            ($ , {I0, ?STRIDE, ?STRIDE}) -> I = I0+1, J = 0, {start (I, J, false), {I, J+1, ?STRIDE}};
                            ($ , {I,        J, ?STRIDE}) ->                  {start (I, J, false), {I, J+1, ?STRIDE}} end,
                        {0, 0, ?STRIDE}, X),
    Pids.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start stimulus unit
%% @end
%%--------------------------------------------------------------------
-spec start (Row, Column, Active) -> pid () when
      Row    :: non_neg_integer (),
      Column :: non_neg_integer (),
      Active :: boolean ().

start (I, J, Active) ->
    erlang:spawn (?MODULE, loop, [#state{network  = self (),
                                         id       = I * ?STRIDE + J,
                                         active   = Active,
                                         position = {I * ?STEP - ?LINES * ?STEP / 2 + ?STEP / 2,
                                                     -300.0,
                                                     J * ?STEP - ?STRIDE * ?STEP / 2 + ?STEP / 2}}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stimulus message processing loop
%% @end
%%--------------------------------------------------------------------
-spec loop (#state{}) -> no_return ().

loop (#state{id = Id, position = Position, timer_ref = Timer} = State) ->
    NewState = receive
        sim_start ->
            ok = vnn_event:send_stimulus_pos (Id, Position),
            NewTimer = erlang:start_timer (next_spike_time (State), self (), spike),
            State#state{timer_ref = NewTimer};

        sim_stop ->
            case is_reference (Timer) of true  -> erlang:cancel_timer (Timer);
                                         false -> ok end,
            State;

        {timeout, Timer, spike} ->
            ok = vnn_event:send_stimulus_spike (Id),
            NewTimer = erlang:start_timer (next_spike_time (State), self (), spike),
            State#state{timer_ref = NewTimer};

        {timeout, _, _} ->
            throw (unexpected_timeout);

        {get_state, Pid} ->
            Pid ! State,
            State
    end,
    vnn_stimulus:loop (NewState).

%%--------------------------------------------------------------------
-spec next_spike_time (#state{}) -> non_neg_integer ().

next_spike_time (#state{absolute_refractory = Refractory, active = Active, noise_rate = NoiseRate, spike_rate = SpikeRate}) ->
    case Active of
        true  -> to_millis (vnn_distribution:exponential (SpikeRate)) + Refractory;
        false -> to_millis (vnn_distribution:exponential (NoiseRate)) + Refractory
    end.

%%--------------------------------------------------------------------
-spec to_millis (float ()) -> non_neg_integer ().

to_millis (SecondFraction) -> round (SecondFraction * 1000).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

get_state (Pid) -> Pid ! {get_state, self ()}, receive X -> X end.

spawn_test () ->
    ?assert (is_pid (start (0, 0, true))),
    Pids = start (),
    ?assertEqual (?STRIDE * ?LINES, length (Pids)),
    ?assert (is_pid (lists:nth (1, Pids))),
    ?assertNot ((get_state (lists:nth (1, Pids)))#state.active),
    ?assertNot ((get_state (lists:nth (1, Pids)))#state.active),
    ?assertEqual ({-190.0, -300.0, -390.0}, (get_state (lists:nth (1, Pids)))#state.position),
    ?assertEqual ({190.0, -300.0, 390.0}, (get_state (lists:nth (?STRIDE * ?LINES, Pids)))#state.position),
    ?assert ((get_state (lists:nth (125, Pids)))#state.active).

nif_test () ->
    ?assert (vnn_distribution:exponential (1.0) >= 0.0).

-endif.
