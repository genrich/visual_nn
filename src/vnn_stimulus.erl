%%--------------------------------------------------------------------
%% @doc
%% Stimulus
%% @end
%%--------------------------------------------------------------------
-module (vnn_stimulus).

-export ([start/0, loop/1]).

-include_lib ("lager/include/lager.hrl").

-record (state, {absolute_refractory = 1            :: pos_integer (),
                 active                             :: boolean (),
                 connections_outbound = sets:new () :: sets:set (),
                 id                                 :: non_neg_integer (),
                 network                            :: pid (),
                 noise_rate = 0.01                  :: float (),
                 position                           :: vnn_network:position (),
                 simulation                         :: boolean (),
                 spike_rate = 1.0                   :: float (),
                 timer_ref                          :: reference (),
                 time_factor = 1.0                  :: float ()}).

-define (STRIDE, 40).
-define (LINES,  20).
-define (STEP,   14).

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
    spawn (?MODULE, loop, [#state{network  = self (),
                                  id       = I * ?STRIDE + J,
                                  active   = Active,
                                  position = {I * ?STEP - ?LINES * ?STEP / 2 + ?STEP / 2,
                                              -300.0,
                                              (?STRIDE - 1 - J) * ?STEP - ?STRIDE * ?STEP / 2 + ?STEP / 2}}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stimulus message processing loop
%% @end
%%--------------------------------------------------------------------
-spec loop (#state{}) -> no_return ().

loop (#state{id = StimulusId, position = StimulusPos, connections_outbound = ConnsOut, timer_ref = Timer} = State) ->
    NewState = receive
        sim_start ->
            ok = vnn_event:send_stimulus_pos (StimulusId, StimulusPos),
            sets:fold (fun ({NeuronPid, ConnId}, _) -> NeuronPid ! {send_conn, StimulusPos, ConnId}, {} end, {}, ConnsOut),

            NextSpike = erlang:start_timer (next_spike_time (State), self (), spike),
            State#state{timer_ref = NextSpike};

        sim_stop ->
            case is_reference (Timer) of true  -> erlang:cancel_timer (Timer);
                                         false -> ok end,
            %% vnn_network:propagate (ConnsOut, sim_stop),
            State;

        {connect_outbound, Neuron, ConnId} ->
            State#state{connections_outbound = sets:add_element ({Neuron, ConnId}, ConnsOut)};

        {timeout, Timer, spike} ->
            Connections = sets:fold (fun ({_, ConnId}, Acc) -> [ConnId | Acc] end, [], ConnsOut),
            ok = vnn_event:send_stimulus_spike (StimulusId),
            NewTimer = erlang:start_timer (next_spike_time (State), self (), spike),
            State#state{timer_ref = NewTimer};

        {get_state, Pid} ->
            Pid ! State,
            State;

        Msg ->
            throw ({unknown_message, Msg})
    end,
    vnn_stimulus:loop (NewState).

%%--------------------------------------------------------------------
-spec next_spike_time (#state{}) -> non_neg_integer ().

next_spike_time (#state{active = true, absolute_refractory = Refractory, spike_rate = SpikeRate}) ->
    to_millis (vnn_distribution:exponential (SpikeRate)) + Refractory;

next_spike_time (#state{active = false, absolute_refractory = Refractory, noise_rate = NoiseRate}) ->
    to_millis (vnn_distribution:exponential (NoiseRate)) + Refractory.

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
    ?assertEqual ({-133.0, -300.0, 273.0}, (get_state (lists:nth (1, Pids)))#state.position),
    ?assertEqual ({133.0, -300.0, -273.0}, (get_state (lists:nth (?STRIDE * ?LINES, Pids)))#state.position),
    ?assert ((get_state (lists:nth (125, Pids)))#state.active).

nif_test () ->
    ?assert (vnn_distribution:exponential (1.0) >= 0.0).

-endif.
