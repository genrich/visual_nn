%%--------------------------------------------------------------------------------------------------
%% @doc
%% Neuron segment
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_node).

-export ([create/3,
          create/4,
          connect/2,
          start/1,
          stop/1,
          notify_selected/1,
          consider_neighbours/2,
          loop/1]).

-include ("defs.hrl").

-type neighbours_list () :: [{Length :: float (), Node :: pid ()}].

-record (s, {id                       :: non_neg_integer (),
             soma                     :: pid (),
             type                     :: vnn_network:node_type (),
             position                 :: vnn_network:position (),
             node_to_length = #{}     :: #{pid () => float ()},
             is_active = false        :: boolean (),
             sub_nodes  = sets:new () :: sets:set (),
             inbound    = sets:new () :: sets:set (),
             outbound   = sets:new () :: sets:set (),
             neighbours = []          :: neighbours_list ()}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Create node
%% @end
%%--------------------------------------------------------------------------------------------------
-spec create (non_neg_integer (), vnn_network:node_type (), vnn_network:position ()) -> pid ().
%%--------------------------------------------------------------------------------------------------
create (Id, Type, Position) ->
    vnn_event:notify_position (Id, Type, Position),
    Node = spawn (vnn_node, loop, [#s{id = Id, type = Type, position = Position}]),
    Node ! soma_init,
    Node.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Create node
%% @end
%%--------------------------------------------------------------------------------------------------
-spec create (non_neg_integer (), pid (), vnn_network:node_type (), vnn_network:position ()) -> pid ().
%%--------------------------------------------------------------------------------------------------
create (Id, Soma, Type, Position) ->
    vnn_event:notify_position (Id, Type, Position),
    Node = spawn (vnn_node, loop, [#s{id = Id, soma = Soma, type = Type, position = Position}]),
    Soma ! {add_sub_node, Node},
    Node.


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
%% Start stimulus activity
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start (pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
start (Stimulus) ->
    Stimulus ! start,
    ok.


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stop stimulus activity
%% @end
%%--------------------------------------------------------------------------------------------------
-spec stop (pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
stop (Stimulus) ->
    Stimulus ! stop,
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
         soma           = Soma,
         type           = Type,
         position       = Position}
      = S) ->
    NewState =
    receive
        spike when S#s.is_active ->
            process_spike (Type, Id, S#s.node_to_length, S#s.outbound),
            S;

        spike ->
            S;

        start when Type == stimulus_active;
                   Type == stimulus_rest ->
            spike_after (vnn_stimulus:next_spike (Type)),
            S#s{is_active = true};

        start when S#s.is_active == false ->
            S#s{is_active = true};

        start ->
            S;

        stop when S#s.is_active ->
            S#s{is_active = false};

        stop ->
            S;

        soma_init ->
            S#s{soma = self ()};

        notify_selected ->
            [NodeB ! {notify_inbound,  S#s.soma, S#s.id}  || NodeB <- sets:to_list (S#s.inbound)],
            [NodeB ! {notify_outbound, S#s.soma, S#s.id}  || NodeB <- sets:to_list (S#s.outbound)],
            [NodeB ! notify_neighbour || {_, NodeB} <- S#s.neighbours],
            S;

        {notify_inbound, Soma, IdB} ->
            Id = S#s.id,
            vnn_event:notify_inbound (Id, IdB),
            [NodeB ! {notify_inbound, S#s.soma, Id} || NodeB <- sets:to_list (S#s.inbound)],
            S;

        {notify_inbound, _, IdB} ->
            vnn_event:notify_inbound (S#s.id, IdB),
            S;

        {notify_outbound, Soma, IdA} ->
            Id = S#s.id,
            vnn_event:notify_outbound (IdA, Id),
            [NodeB ! {notify_outbound, Soma, Id} || NodeB <- sets:to_list (S#s.outbound)],
            S;

        {notify_outbound, _, IdB} ->
            vnn_event:notify_outbound (S#s.id, IdB),
            S;

        notify_neighbour ->
            vnn_event:notify_neighbour (S#s.id),
            S;

        {add_sub_node, Node} ->
            S#s{sub_nodes = sets:add_element (Node, S#s.sub_nodes)};

        {connect_b, NodeB} ->
            NodeB ! {connect_a, self (), Position},
            S;

        {connect_a, NodeA, PositionA} ->
            Length = length (PositionA, Position),
            NodeA ! {connect_b, self (), Id, Length},
            S#s{inbound = sets:add_element (NodeA, S#s.inbound)};

        {connect_b, NodeB, NodeBId, Length} ->
            %% put (NodeB, Length),
            vnn_event:notify_connection (Id, NodeBId),
            S#s{outbound       = sets:add_element (NodeB, S#s.outbound),
                node_to_length = maps:put (NodeB, Length, S#s.node_to_length)};

        {neighbour_b, NodeB} ->
            NodeB ! {neighbour_a, self (), Position},
            S;

        {neighbour_a, NodeA, NodeAPosition} ->
            Length = length (NodeAPosition, Position),
            NodeA ! {neighbour_b, self (), Length},
            Neighbours = S#s.neighbours,
            S#s{neighbours = consider_neighbour (Length, NodeA, Neighbours, length (Neighbours))};

        {neighbour_b, NodeB, Length} ->
            Neighbours = S#s.neighbours,
            S#s{neighbours = consider_neighbour (Length, NodeB, Neighbours, length (Neighbours))};

        {state, ReplyTo} ->
            ReplyTo ! S,
            S;

        Msg ->
            throw ({unknown_message, Msg})
    end,
    vnn_node:loop (NewState).


%%--------------------------------------------------------------------------------------------------
-spec spike_after (float ()) -> ok.
%%--------------------------------------------------------------------------------------------------
spike_after (Time) ->
    spike_after (Time, self ()).


%%--------------------------------------------------------------------------------------------------
-spec spike_after (float (), pid ()) -> ok.
%%--------------------------------------------------------------------------------------------------
spike_after (Time, Node) ->
    Millis = to_millis (Time),
    case Millis < 10 of
        true  -> Node ! spike;
        false -> erlang:send_after (Millis, Node, spike)
    end,
    ok.


%%--------------------------------------------------------------------------------------------------
-spec process_spike (vnn_network:node_type (), non_neg_integer (), #{pid () => float ()}, sets:set ()) -> ok.
%%--------------------------------------------------------------------------------------------------
process_spike (Type, Id, NodeToLength, Outbound) when Type =:= dendrite; Type =:= axon ->
    propagate_spikes (Id, Outbound, NodeToLength);

process_spike (soma, Id, NodeToLength, Outbound) ->
    case vnn_random:uniform () < 0.1 of true  -> propagate_spikes (Id, Outbound, NodeToLength);
                                        false -> ok
    end;

process_spike (Type, Id, NodeToLength, Outbound) -> % Type is stimulus
    propagate_spikes (Id, Outbound, NodeToLength),
    spike_after (vnn_stimulus:next_spike (Type) + vnn_params:absolute_refractory ()).


%%--------------------------------------------------------------------------------------------------
-spec propagate_spikes (non_neg_integer (), sets:set (), #{pid () => float ()}) -> ok.
%%--------------------------------------------------------------------------------------------------
propagate_spikes (NodeAId, Outbound, NodeToLength) ->
    vnn_event:notify_spike (NodeAId),
    F = fun (NodeB) ->
                %% Length = get (NodeB),
                Length = maps:get (NodeB, NodeToLength),
                spike_after (Length / vnn_params:spike_speed (), NodeB)
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
            Node1 = create (vnn_utils:id (), soma, Position),
            Node2 = create (vnn_utils:id (), soma, Position),
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
            [Node | Nodes] = [create (vnn_utils:id (), soma, {NodeId - 1, 0, 0}) || NodeId <- lists:seq (1, ?MAX_NEIGHBOURS + 1)],
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
