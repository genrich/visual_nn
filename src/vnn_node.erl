%%--------------------------------------------------------------------------------------------------
%% @doc
%% Neuron segment
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_node).

-export ([create/2,
          connect/2,
          notify_selected/1,
          notify_position/1,
          notify_connections/1,
          consider_neighbours/2,
          loop/1]).

-include ("vnn_const.hrl").

-type neighbours_list () :: [{Length :: float (), Node :: pid ()}].

-record (s, {id                       :: non_neg_integer (),
             type                     :: vnn_network:node_type (),
             position                 :: vnn_network:position (),
             node_to_length = #{}     :: #{pid () => float ()},
             is_simulation            :: boolean (),
             inbound    = sets:new () :: sets:set (),
             outbound   = sets:new () :: sets:set (),
             neighbours = []          :: neighbours_list ()}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Create node
%% @end
%%--------------------------------------------------------------------------------------------------
-spec create (Type :: vnn_network:node_type (), Position :: vnn_network:position ()) -> no_return ().
%%--------------------------------------------------------------------------------------------------
create (Type, Position) ->
    Id = vnn_utils:id (),
    vnn_network:register_node (Id, self ()),
    vnn_node:loop (#s{id = Id, type = Type, position = Position}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Connect Nodes
%% @end
%%--------------------------------------------------------------------------------------------------
-spec connect (FromPid :: pid (), ToPid :: pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
connect (FromPid, ToPid) ->
    FromPid ! {connect_b, ToPid},
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Make node notify event handler about its inbound/outbound/neighbours
%% @end
%%--------------------------------------------------------------------------------------------------
-spec notify_selected (pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
notify_selected (Node) ->
    Node ! notify_selected,
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Make node notify event handler about its position
%% @end
%%--------------------------------------------------------------------------------------------------
-spec notify_position (pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
notify_position (Node) ->
    Node ! notify_position,
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Make node notify event handler about its outbound connections
%% @end
%%--------------------------------------------------------------------------------------------------
-spec notify_connections (pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
notify_connections (Node) ->
    Node ! notify_connections,
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Consider nodes to be neighbours
%% @end
%%--------------------------------------------------------------------------------------------------
-spec consider_neighbours (pid (), pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
consider_neighbours (PidA, PidB) ->
    PidA ! {neighbour_b, PidB},
    ok.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Node message processing loop
%% @end
%%--------------------------------------------------------------------------------------------------
-spec loop (#s{}) -> no_return ().
%%--------------------------------------------------------------------------------------------------
loop (#s{id             = Id,
         position       = Position,
         node_to_length = NodeToLength,
         inbound        = Inbound,
         outbound       = Outbound,
         neighbours     = Neighbours}
      = State) ->
    NewState =
    receive
        spike ->
            spike (State);

        neighbour_spike ->
            State;

        sim_start ->
            generate_stimulus_spike (State#s{is_simulation = true});

        sim_stop ->
            State#s{is_simulation = false};

        notify_selected ->
            [NodeB ! notify_inbound   || NodeB      <- sets:to_list (Inbound)],
            [NodeB ! notify_outbound  || NodeB      <- sets:to_list (Outbound)],
            [NodeB ! notify_neighbour || {_, NodeB} <- Neighbours],
            State;

        notify_inbound ->
            ok = vnn_event:notify_inbound (Id),
            State;

        notify_outbound ->
            ok = vnn_event:notify_outbound (Id),
            State;

        notify_neighbour ->
            ok = vnn_event:notify_neighbour (Id),
            State;

        notify_position ->
            ok = vnn_event:notify_position (Id, Position),
            State;

        notify_connections ->
            [NodeB ! {notify_connection, Id} || NodeB <- sets:to_list (Outbound)],
            State;

        {notify_connection, NodeA} ->
            ok = vnn_event:notify_connection (NodeA, Id),
            State;

        {connect_b, NodeB} ->
            NodeB ! {connect_a, self (), Position},
            State;

        {connect_a, NodeA, PositionA} ->
            Length = length (PositionA, Position),
            NodeA ! {connect_b, self (), Length},
            State#s{inbound = sets:add_element (NodeA, Inbound)};

        {connect_b, NodeB, Length} ->
            %% put (NodeB, Length),
            State#s{outbound       = sets:add_element (NodeB, Outbound),
                    node_to_length = maps:put (NodeB, Length, NodeToLength)};

        {neighbour_b, NodeB} ->
            NodeB ! {neighbour_a, self (), Position},
            State;

        {neighbour_a, NodeA, NodeAPosition} ->
            Length = length (NodeAPosition, Position),
            NodeA ! {neighbour_b, self (), Length},
            State#s{neighbours = consider_neighbour (Length, NodeA, Neighbours, length (Neighbours))};

        {neighbour_b, NodeB, Length} ->
            State#s{neighbours = consider_neighbour (Length, NodeB, Neighbours, length (Neighbours))};

        {state, ReplyTo} ->
            ReplyTo ! State,
            State;

        Msg ->
            throw ({unknown_message, Msg})
    end,
    vnn_node:loop (NewState).

%%--------------------------------------------------------------------------------------------------
-spec generate_stimulus_spike (#s{}) -> #s{}.
%%--------------------------------------------------------------------------------------------------
generate_stimulus_spike (#s{type = stimulus_active} = State) ->
    erlang:send_after (to_millis (vnn_random:exponential (vnn_params:spike_rate ())
                                  + vnn_params:absolute_refractory ()),
                       self (),
                       spike),
    State;

generate_stimulus_spike (#s{type = stimulus} = State) ->
    erlang:send_after (to_millis (vnn_random:exponential (vnn_params:noise_rate ())
                                  + vnn_params:absolute_refractory ()),
                       self (),
                       spike),
    State.


%%--------------------------------------------------------------------------------------------------
-spec spike (#s{}) -> #s{}.
%%--------------------------------------------------------------------------------------------------
spike (#s{type           = stimulus_active,
          is_simulation  = true,
          id             = Id,
          node_to_length = NodeToLength,
          outbound       = Outbound}
       = State) ->
    propagate_spikes (Id, Outbound, NodeToLength),
    generate_stimulus_spike (State);

spike (#s{type           = stimulus,
          is_simulation  = true,
          id             = Id,
          node_to_length = NodeToLength,
          outbound       = Outbound}
       = State) ->
    propagate_spikes (Id, Outbound, NodeToLength),
    generate_stimulus_spike (State);

spike (#s{type           = neuron,
          id             = Id,
          node_to_length = NodeToLength,
          outbound       = Outbound}
       = State) ->
    case vnn_random:uniform () < 0.1 of true  -> propagate_spikes (Id, Outbound, NodeToLength);
                                        false -> undefined
    end,
    State;

spike (#s{is_simulation = false} = State) ->
    State;

spike (#s{} = State) ->
    State.


%%--------------------------------------------------------------------------------------------------
-spec propagate_spikes (non_neg_integer (), sets:set (), #{pid () => float ()}) -> ok.
%%--------------------------------------------------------------------------------------------------
propagate_spikes (NodeAId, Outbound, NodeToLength) ->
    ok = vnn_event:notify_spike (NodeAId),
    F = fun (NodeB) ->
                %% Length = get (NodeB),
                Length = maps:get (NodeB, NodeToLength),
                SpikeTravelDuration = to_millis (Length / vnn_params:spike_speed ()),
                erlang:send_after (SpikeTravelDuration, NodeB, spike)
        end,
    [F (NodeB) || NodeB <- sets:to_list (Outbound)],
    ok.


%%--------------------------------------------------------------------------------------------------
-spec consider_neighbour (float (), pid (), neighbours_list (), non_neg_integer ()) -> neighbours_list ().
%%--------------------------------------------------------------------------------------------------
consider_neighbour (Length, Node, Neighbours, ?MAX_NEIGHBOURS) ->
    [{LargestLength, _} | Tail] = lists:sort (fun ({LengthA, _}, {LengthB, _}) -> LengthA > LengthB end,
                                              Neighbours),
    case Length < LargestLength of
        true  -> [{Length, Node} | Tail];
        false -> Neighbours
    end;

consider_neighbour (Length, Node, Neighbours, _) ->
    [{Length, Node} | Neighbours].


%%--------------------------------------------------------------------------------------------------
-spec to_millis (float ()) -> non_neg_integer ().
%%--------------------------------------------------------------------------------------------------
to_millis (Seconds) -> round (Seconds * 1000).


%%--------------------------------------------------------------------------------------------------
-spec length (FromPosition :: vnn_network:position (), ToPosition :: vnn_network:position ()) -> float ().
%%--------------------------------------------------------------------------------------------------
length ({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    math:sqrt (pow2 (X1 - X2) + pow2 (Y1 - Y2) + pow2 (Z1 - Z2)).


%%--------------------------------------------------------------------------------------------------
-spec pow2 (float ()) -> float ().
%%--------------------------------------------------------------------------------------------------
pow2 (A) -> A * A.


%%--------------------------------------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
node_test_ () ->
    {setup,
     fun () ->
             vnn_utils:start_link (),
             vnn_event:start_link ()
     end,
     [{"connection", ?_test
       (begin
            Position = {0, 0, 0},
            Node1 = spawn (vnn_node, create, [neuron, Position]),
            Node2 = spawn (vnn_node, create, [neuron, Position]),
            ?assert (is_process_alive (Node1)),
            ?assert (is_process_alive (Node2)),

            connect (Node1, Node2),
            timer:sleep (300),

            Node1 ! {state, self ()},
            receive State1 -> State1 end,
            Node2 ! {state, self ()},
            receive State2 -> State2 end,

            ?assert (sets:is_element (Node2, State1#s.outbound)),
            ?assert (sets:is_element (Node1, State2#s.inbound))
        end)},

      {"neighbours", ?_test
       (begin
            [Node | Nodes] = [spawn (vnn_node, create, [neuron, {NodeId - 1, 0, 0}]) || NodeId <- lists:seq (1, ?MAX_NEIGHBOURS + 1)],
            [consider_neighbours (Node, OtherNode) || OtherNode <- Nodes],
            timer:sleep (300),

            Node ! {state, self ()},
            receive State -> State end,

            ?assert (length (State#s.neighbours) == ?MAX_NEIGHBOURS),
            {Neighbours, NotNeighbour} = lists:split (?MAX_NEIGHBOURS, Nodes),
            ?assertNot (lists:keyfind (NotNeighbour, 2, State#s.neighbours)),
            [?assert (is_tuple (lists:keyfind (Neighbour, 2, State#s.neighbours))) || Neighbour <- Neighbours]
        end)}]}.
-endif.
