%%--------------------------------------------------------------------
%% @doc
%% Neuron segment
%% @end
%%--------------------------------------------------------------------
-module (vnn_node).

-export ([create/2, loop/1]).

-record (s, {id                     :: non_neg_integer (),
             type                   :: vnn_network:node_type (),
             position               :: vnn_network:position (),
             is_simulation          :: boolean (),
             inbound  = sets:new () :: sets:set (),
             outbound = sets:new () :: sets:set ()}).

%%--------------------------------------------------------------------
%% @doc
%% Create node
%% @end
%%--------------------------------------------------------------------
-spec create (Type :: vnn_network:node_type (), Position :: vnn_network:position ()) -> no_return ().

create (Type, Position) ->
    vnn_node:loop (#s{id = vnn_utils:id (), type = Type, position = Position}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Node message processing loop
%% @end
%%--------------------------------------------------------------------
-spec loop (#s{}) -> no_return ().

loop (#s{id = Id, position = Position, inbound = Inbound, outbound = Outbound} = State) ->
    NewState =
    receive
        spike ->
            spike (State);

        sim_start ->
            generate_stimulus_spike (State#s{is_simulation = true});

        sim_stop ->
            State#s{is_simulation = false};

        notify_position ->
            ok = vnn_event:notify_position (Id, Position),
            State;

        notify_connections ->
            [To ! {notify_connection, Id} || To <- sets:to_list (Outbound)],
            State;

        {notify_connection, FromId} ->
            ok = vnn_event:notify_connection (FromId, Id),
            State;

        {request_connection_to, To} ->
            To ! {connect_from, self (), Position},
            State;

        {connect_from, From, FromPosition} ->
            Length = length (FromPosition, Position),
            put (From, Length),
            From ! {connect_to, self (), Length},
            State#s{inbound = sets:add_element (From, Inbound)};

        {connect_to, To, Length} ->
            put (To, Length),
            State#s{outbound = sets:add_element (To, Outbound)};

        Msg ->
            throw ({unknown_message, Msg})
    end,
    vnn_node:loop (NewState).

-define (ABSOLUTE_REFRACTORY, 1).
-define (NOISE_RATE,          0.01).
-define (SPIKE_RATE,          1.0).

%%--------------------------------------------------------------------
-spec generate_stimulus_spike (#s{}) -> #s{}.

generate_stimulus_spike (#s{type = stimulus_active, is_simulation = true} = State) ->
    NextSpikeTime = to_millis (vnn_random:exponential (?SPIKE_RATE)) + ?ABSOLUTE_REFRACTORY,
    erlang:send_after (NextSpikeTime, self (), spike),
    State;

generate_stimulus_spike (#s{type = stimulus, is_simulation = true} = State) ->
    NextSpikeTime = to_millis (vnn_random:exponential (?NOISE_RATE)) + ?ABSOLUTE_REFRACTORY,
    erlang:send_after (NextSpikeTime, self (), spike),
    State.

%%--------------------------------------------------------------------
-spec spike (#s{}) -> #s{}.

spike (#s{id = Id, type = stimulus_active, is_simulation = true, outbound = Outbound} = State) ->
    propagate_spikes (Id, Outbound),
    generate_stimulus_spike (State);

spike (#s{id = Id, type = stimulus, is_simulation = true, outbound = Outbound} = State) ->
    propagate_spikes (Id, Outbound),
    generate_stimulus_spike (State);

spike (#s{id = Id, type = neuron, outbound = Outbound} = State) ->
    case vnn_random:uniform () < 0.1 of true  -> propagate_spikes (Id, Outbound);
                                        false -> undefined
    end,
    State;

spike (#s{is_simulation = false} = State) ->
    State;

spike (#s{} = State) ->
    State.

%%--------------------------------------------------------------------
-spec propagate_spikes (Id :: non_neg_integer (), Outbound :: sets:set ()) -> ok.

propagate_spikes (Id, Outbound) ->
    ok = vnn_event:notify_spike (Id),
    F = fun (Pid) ->
                Length = get (Pid),
                NextSpikeTime = to_millis (Length / vnn_params:spike_speed ()) + ?ABSOLUTE_REFRACTORY,
                erlang:send_after (NextSpikeTime, Pid, spike)
        end,
    [F (To) || To <- sets:to_list (Outbound)],
    ok.

%%--------------------------------------------------------------------
-spec to_millis (float ()) -> non_neg_integer ().

to_millis (SecondFraction) -> round (SecondFraction * 1000).

%%--------------------------------------------------------------------
-spec length (FromPosition :: vnn_network:position (), ToPosition :: vnn_network:position ()) -> float ().

length ({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    math:sqrt (pow2 (X1 - X2) + pow2 (Y1 - Y2) + pow2 (Z1 - Z2)).

%%--------------------------------------------------------------------
-spec pow2 (float ()) -> float ().

pow2 (A) -> A * A.
