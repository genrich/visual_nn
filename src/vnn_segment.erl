%%--------------------------------------------------------------------
%% @doc
%% Neuron segment
%% @end
%%--------------------------------------------------------------------
-module (vnn_segment).

-export ([loop/1, start_neurons/0]).

-record (state, {absolute_refractory = 1            :: pos_integer (),
                 connections_outbound = sets:new () :: sets:set (),
                 from                               :: pid (),
                 id                                 :: non_neg_integer (),
                 position                           :: vnn_network:position (),
                 to                                 :: pid (),
                 type                               :: dendrite | soma | axon | axon_term,
                 timer_ref                          :: reference ()}).

start_neurons () ->
    [spawn (?MODULE, loop, [#state{id = vnn_id_pool:id (), type = soma,
                                   position = {-140 + random:uniform (280),
                                               -200 + random:uniform (400),
                                               -280 + random:uniform (560)}}])
     || Count <- lists:seq (0, 9)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Neuron segment message processing loop
%% @end
%%--------------------------------------------------------------------
-spec loop (#state{}) -> no_return ().

loop (#state{id = Id, position = Position} = State) ->
    NewState = receive
        {send_conn, StimulusId} ->
            ok = vnn_event:notify_position (Id, Position),
            ok = vnn_event:notify_connection (StimulusId, Id),
            %% NewTimer = erlang:start_timer (next_spike_time (State), self (), spike),
            %% State#state{timer_ref = NewTimer};
            State;

        sim_stop ->
            %% case is_reference (Timer) of true  -> erlang:cancel_timer (Timer);
            %%                              false -> ok end,
            State;

        Msg ->
            throw ({unknown_message, Msg})
    end,
    vnn_segment:loop (NewState).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

start_neurons_test () ->
    ?assert (is_pid (lists:nth (1, start_neurons ()))),
    ok.

-endif.
