%%--------------------------------------------------------------------------------------------------
%% @doc
%% Main module
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_network).

-export ([start_link/1,
          stop/0,
          sim_start/0,
          sim_stop/0,
          add_node/4,
          add_connection/2,
          select_node/1]).

-export_type ([position/0, node_type/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type position ()  :: {X :: number (), Y :: number (), Z :: number ()}.
-type node_type () :: stimulus_active | stimulus_rest | soma | dendrite | axon.

-include_lib ("lager/include/lager.hrl").

-include ("const.hrl").

-record (s, {nodes = []       :: [pid ()],
             id_to_node = #{} :: #{non_neg_integer () => pid ()}}).

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_link (non_neg_integer ()) -> {ok, pid ()}.
%%--------------------------------------------------------------------------------------------------
start_link (NetworkId) ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, NetworkId, []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stops the server
%% @end
%%--------------------------------------------------------------------------------------------------
-spec stop () -> ok.
%%--------------------------------------------------------------------------------------------------
stop () ->
    gen_server:cast (?MODULE, stop).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start simulation
%% @end
%%--------------------------------------------------------------------------------------------------
-spec sim_start () -> ok.
%%--------------------------------------------------------------------------------------------------
sim_start () ->
    gen_server:cast (?MODULE, sim_start).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stop simulation
%% @end
%%--------------------------------------------------------------------------------------------------
-spec sim_stop () -> ok.
%%--------------------------------------------------------------------------------------------------
sim_stop () ->
    gen_server:cast (?MODULE, sim_stop).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Add node to the network
%% @end
%%--------------------------------------------------------------------------------------------------
-spec add_node (non_neg_integer (), non_neg_integer (), node_type (), position ()) -> ok.
%%--------------------------------------------------------------------------------------------------
add_node (Id, SomaId, Type, Position) ->
    gen_server:cast (?MODULE, {add_node, {Id, SomaId, Type, Position}}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Add connection to the network
%% @end
%%--------------------------------------------------------------------------------------------------
-spec add_connection (non_neg_integer (), non_neg_integer ()) -> ok.
%%--------------------------------------------------------------------------------------------------
add_connection (NodeAId, NodeBId) ->
    gen_server:cast (?MODULE, {add_connection, {NodeAId, NodeBId}}).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Process node selection: notify all node neighbours and connections
%% @end
%%--------------------------------------------------------------------------------------------------
-spec select_node (non_neg_integer ()) -> ok.
%%--------------------------------------------------------------------------------------------------
select_node (Id) ->
    gen_server:cast (?MODULE, {select_node, Id}).


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the neural network with new Stimuli and Neurons
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init (non_neg_integer ()) -> {ok, #s{}}.
%%--------------------------------------------------------------------------------------------------
init (?NETWORK_0) ->
    lager:debug ("network init 0"),
    process_flag (trap_exit, true),

    vnn_event:notify_new_network (),

    vnn_utils:reset_id (),

    State = #s{},
    {Stimuli, State1} = create_stimulus (vnn_stimulus:hello_world (), State),
    {Layer1,  State2} = create_layer (1, 100, State1),
    {Layer2,  State3} = create_layer (2, 100, State2),
    {Layer3,  State4} = create_layer (3, 100, State3),
    {Layer4,  State5} = create_layer (4, 100, State4),
    {Layer5,  State6} = create_layer (5, 100, State5),
    {Layer6,  State7} = create_layer (6, 100, State6),

    [vnn_node:connect (Stimulus, Node4) || Stimulus <- Stimuli, Node4 <- Layer4, vnn_random:uniform () < 0.003],
    [vnn_node:connect (Stimulus, Node6) || Stimulus <- Stimuli, Node6 <- Layer6, vnn_random:uniform () < 0.0005],

    [vnn_node:connect (Node4_1, Node4_2) || Node4_1 <- Layer4, Node4_2 <- Layer4, Node4_1 =/= Node4_2, vnn_random:uniform () < 0.01],

    [vnn_node:connect (Node6, Node4) || Node6 <- Layer6, Node4 <- Layer4, vnn_random:uniform () < 0.01],

    [vnn_node:connect (Node4, Node2) || Node4 <- Layer4, Node2 <- Layer2, vnn_random:uniform () < 0.01],
    [vnn_node:connect (Node4, Node3) || Node4 <- Layer4, Node3 <- Layer3, vnn_random:uniform () < 0.01],

    [vnn_node:connect (Node2, Node1) || Node2 <- Layer2, Node1 <- Layer1, vnn_random:uniform () < 0.01],
    [vnn_node:connect (Node2, Node6) || Node2 <- Layer2, Node6 <- Layer6, vnn_random:uniform () < 0.01],

    [vnn_node:connect (Node3, Node5) || Node3 <- Layer3, Node5 <- Layer5, vnn_random:uniform () < 0.01],

    Layers = Layer1 ++ Layer2 ++ Layer3 ++ Layer4 ++ Layer5 ++ Layer6,
    neighbours (Layers),

    {ok, State7};

init (?NETWORK_1) ->
    lager:debug ("network init 1"),
    process_flag (trap_exit, true),

    vnn_event:notify_new_network (),

    vnn_utils:reset_id (),

    State = #s{},
    {Stimuli, State1} = create_stimulus (vnn_stimulus:hello_world (), State),
    {Layer, State2} = create_layer (3, 100, State1),

    [vnn_node:connect (Stimulus, Node1) || Stimulus <- Stimuli, Node1 <- Layer, vnn_random:uniform () < 0.005],

    neighbours (Layer),

    {ok, State2};

init (?NETWORK_2) ->
    lager:debug ("network init 2"),
    process_flag (trap_exit, true),

    vnn_event:notify_new_network (),

    vnn_utils:reset_id (),

    State = #s{},
    {Stimulus, State1} = create_node (stimulus_active, {0, -300, 0}, State),
    {Node,     State2} = create_node (soma,            {0,  300, 0}, State1),

    vnn_node:connect (Stimulus, Node),

    {ok, State2};

init (?NETWORK_3) ->
    lager:debug ("network init 3"),
    process_flag (trap_exit, true),

    vnn_event:notify_new_network (),

    vnn_cnode:create_network (),

    {ok, #s{}}.

%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_call (Request :: term (), From :: {pid (), Tag :: term ()}, State :: #s{}) ->
    {reply, Reply :: term (), NewState :: #s{}}                         |
    {reply, Reply :: term (), NewState :: #s{}, timeout () | hibernate} |
    {noreply, NewState :: #s{}}                                         |
    {noreply, NewState :: #s{}, timeout () | hibernate}                 |
    {stop, Reason :: term (), Reply :: term (), NewState :: #s{}}       |
    {stop, Reason :: term (), NewState :: #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_cast (term (), #s{}) -> {noreply, #s{}} | {stop, normal, #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_cast (sim_start, #s{nodes = Nodes} = State) ->
    lager:debug ("sim_start"),
    [vnn_node:start (Node) || Node <- Nodes],
    {noreply, State};

handle_cast (sim_stop, #s{nodes = Nodes} = State) ->
    lager:debug ("sim_stop"),
    [vnn_node:stop (Node) || Node <- Nodes],
    {noreply, State};

% soma, stimulus_active, stimulus_rest will send Id == SomaId
handle_cast ({add_node, {Id, Id, Type, Position}}, #s{nodes = Nodes, id_to_node = IdToNode} = State) ->
    Node = vnn_node:create (Id, Type, Position),
    {noreply, State#s{nodes = [Node | Nodes], id_to_node = maps:put (Id, Node, IdToNode)}};

handle_cast ({add_node, {Id, SomaId, Type, Position}}, #s{nodes = Nodes, id_to_node = IdToNode} = State) ->
    Soma = maps:get (SomaId, IdToNode),
    Node = vnn_node:create (Id, Soma, Type, Position),
    {noreply, State#s{nodes = [Node | Nodes], id_to_node = maps:put (Id, Node, IdToNode)}};

handle_cast ({add_connection, {NodeAId, NodeBId}}, #s{id_to_node = IdToNode} = State) ->
    NodeA = maps:get (NodeAId, IdToNode),
    NodeB = maps:get (NodeBId, IdToNode),
    vnn_node:connect (NodeA, NodeB),
    {noreply, State};

handle_cast ({select_node, Id}, #s{id_to_node = IdToNode} = State) ->
    Node = maps:get (Id, IdToNode),
    vnn_node:notify_selected (Node),
    {noreply, State};

handle_cast (stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_info (timeout, #s{}) -> {noreply, #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_info (timeout, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------------------------------------
-spec terminate (Reason :: (normal | shutdown | {shutdown, term ()} | term ()),
                 State :: term ()) -> term ().
%%--------------------------------------------------------------------------------------------------
terminate (Reason, #s{nodes = Nodes}) ->
    lager:debug ("network terminate: ~p", [Reason]),
    [exit (Node, shutdown) || Node <- Nodes],
    ok.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------------------------------------
-spec code_change (OldVsn :: (term () | {down, term ()}), State :: term (), Extra :: term ()) ->
    {ok, #s{}} |
    {error, Reason :: term ()}.
%%--------------------------------------------------------------------------------------------------
code_change (_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------------------------------------
-spec create_layer (non_neg_integer (), pos_integer (), #s{}) -> {[pid ()], #s{}}.
%%--------------------------------------------------------------------------------------------------
create_layer (LayerId, Count, State) when Count > 0 ->
    lists:mapfoldl (fun (_, S) -> create_neuron (LayerId, S) end, State, lists:seq (1, Count)).


%%--------------------------------------------------------------------------------------------------
-spec create_neuron (pos_integer (), #s{}) -> {pid (), #s{}}.
%%--------------------------------------------------------------------------------------------------
create_neuron (LayerId, State) ->
    Position = {-140 + vnn_random:uniform (0.0, 280.0),
                 300 - ((LayerId - 1) * 100) + vnn_random:normal (0.0, 10.0),
                -280 + vnn_random:uniform (0.0, 560.0)},
    create_node (soma, Position, State).


%%--------------------------------------------------------------------------------------------------
-spec create_stimulus ({nonempty_string (), pos_integer (), pos_integer ()}, #s{}) -> {[pid ()], #s{}}.
%%--------------------------------------------------------------------------------------------------
create_stimulus ({String, Stride, Lines}, State) ->
    {Pids, {_, _, _, NewState}} =
    lists:mapfoldl (
      fun ($X, {I0, Strd, Strd, S}) -> I = I0 + 1, J = 0,
                                       {N, NewS} = create_stimulus (Stride, Lines, I, J, stimulus_active, S),
                                       {N, {I, J + 1, Strd, NewS}};
          ($X, {I,  J,    Strd, S}) -> {N, NewS} = create_stimulus (Stride, Lines, I, J, stimulus_active, S),
                                       {N, {I, J + 1, Strd, NewS}};
          ($ , {I0, Strd, Strd, S}) -> I = I0 + 1, J = 0,
                                       {N, NewS} = create_stimulus (Stride, Lines, I, J, stimulus_rest, S),
                                       {N, {I, J + 1, Strd, NewS}};
          ($ , {I,  J,    Strd, S}) -> {N, NewS} = create_stimulus (Stride, Lines, I, J, stimulus_rest, S),
                                       {N, {I, J + 1, Strd, NewS}} end,
        {0, 0, Stride, State}, String),
    {Pids, NewState}.


%%--------------------------------------------------------------------------------------------------
-spec create_stimulus (non_neg_integer (),
                       non_neg_integer (),
                       non_neg_integer (),
                       non_neg_integer (),
                       node_type (),
                       #s{}) -> {pid (), #s{}}.
%%--------------------------------------------------------------------------------------------------
create_stimulus (Stride, Lines, Row, Col, Type, State) ->
    Step = 14,
    Position = {Row * Step - Lines * Step / 2 + Step / 2,
                -300.0,
                (Stride - 1 - Col) * Step - Stride * Step / 2 + Step / 2},
    create_node (Type, Position, State).


%%--------------------------------------------------------------------------------------------------
-spec create_node (node_type (), position (), #s{}) -> {pid (), #s{}}.
%%--------------------------------------------------------------------------------------------------
create_node (Type, Position, State) ->
    create_node (vnn_utils:id (), Type, Position, State).


%%--------------------------------------------------------------------------------------------------
-spec create_node (non_neg_integer (), node_type (), position (), #s{}) -> {pid (), #s{}}.
%%--------------------------------------------------------------------------------------------------
create_node (Id, Type, Position, State) ->
    Node = vnn_node:create (Id, Type, Position),
    {Node, State#s{nodes = [Node | State#s.nodes], id_to_node = maps:put (Id, Node, State#s.id_to_node)}}.


%%--------------------------------------------------------------------------------------------------
-spec neighbours ([pid ()]) -> ok.
%%--------------------------------------------------------------------------------------------------
neighbours ([]) ->
    ok;

neighbours ([Node | Nodes]) ->
    [vnn_node:consider_neighbours (Node, OtherNode) || OtherNode <- Nodes],
    neighbours (Nodes).


%%--------------------------------------------------------------------------------------------------
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
network_start_test_ () ->
    {setup,
     fun () ->
             %% application:ensure_all_started (lager),
             %% lager:set_loglevel (lager_console_backend, debug),
             vnn_utils:start_link (),
             vnn_event:start_link ()
     end,
     [?_test
      (begin
           NetworkId = 0,
           {ok, Pid} = start_link (NetworkId),
           ?assert (is_pid (Pid)),
           stop (),
           timer:sleep (300)
       end),
      ?_test
      (begin
           NetworkId = 1,
           {ok, Pid} = start_link (NetworkId),
           ?assert (is_pid (Pid)),
           stop (),
           timer:sleep (300)
       end)]}.

-endif.
