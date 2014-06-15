%%--------------------------------------------------------------------
%% @doc
%% Main module
%% @end
%%--------------------------------------------------------------------
-module (vnn_network).

-export ([start_link/0,
          sim_start/0,
          sim_stop/0,
          set_spike_speed/1,
          createStimulus/5]).

-export_type([position/0, node_type/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type position ()  :: {number (), number (), number ()}.
-type node_type () :: stimulus_active | stimulus | neuron.


-record (state, {nodes   :: [pid ()],
                 stimuli :: [pid ()]}).

-include_lib ("lager/include/lager.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link () -> {ok, pid ()} | ignore | {error,  {already_started, pid ()} | term ()}.

start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Start simulation
%% @end
%%--------------------------------------------------------------------
-spec sim_start () -> ok.

sim_start () ->
    gen_server:cast (?MODULE, sim_start).

%%--------------------------------------------------------------------
%% @doc
%% Stop simulation
%% @end
%%--------------------------------------------------------------------
-spec sim_stop () -> ok.

sim_stop () ->
    gen_server:cast (?MODULE, sim_stop).

%%--------------------------------------------------------------------
%% @doc
%% Set spike speed param
%% @end
%%--------------------------------------------------------------------
-spec set_spike_speed (Speed :: float ()) -> ok.

set_spike_speed (Speed) ->
    gen_server:cast (?MODULE, {set_spike_speed, Speed}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the neural network with new Stimuli and Neurons
%% @end
%%--------------------------------------------------------------------
-spec init ([]) -> {ok, #state{}}.

init ([]) ->
    lager:debug ("network init"),
    process_flag (trap_exit, true),

    vnn_event:notify_new_network (),

    vnn_utils:reset_id (),

    Stimuli = vnn_stimulus:create (),
    Layer1 = create_layer (1, 100),
    Layer2 = create_layer (2, 100),
    Layer3 = create_layer (3, 100),
    Layer4 = create_layer (4, 100),
    Layer5 = create_layer (5, 100),
    Layer6 = create_layer (6, 100),

    [connect (Stimulus, Node4) || Stimulus <- Stimuli, Node4 <- Layer4, vnn_random:uniform () < 0.003],
    [connect (Stimulus, Node6) || Stimulus <- Stimuli, Node6 <- Layer6, vnn_random:uniform () < 0.0005],

    [connect (Node4_1, Node4_2) || Node4_1 <- Layer4, Node4_2 <- Layer4, Node4_1 =/= Node4_2, vnn_random:uniform () < 0.01],

    [connect (Node6, Node4) || Node6 <- Layer6, Node4 <- Layer4, vnn_random:uniform () < 0.01],

    [connect (Node4, Node2) || Node4 <- Layer4, Node2 <- Layer2, vnn_random:uniform () < 0.01],
    [connect (Node4, Node3) || Node4 <- Layer4, Node3 <- Layer3, vnn_random:uniform () < 0.01],

    [connect (Node2, Node1) || Node2 <- Layer2, Node1 <- Layer1, vnn_random:uniform () < 0.01],
    [connect (Node2, Node6) || Node2 <- Layer2, Node6 <- Layer6, vnn_random:uniform () < 0.01],

    [connect (Node3, Node5) || Node3 <- Layer3, Node5 <- Layer5, vnn_random:uniform () < 0.01],

    Nodes = Stimuli ++ Layer1 ++ Layer2 ++ Layer3 ++ Layer4 ++ Layer5 ++ Layer6,
    [Node ! notify_position    || Node <- Nodes],
    [Node ! notify_connections || Node <- Nodes],

    {ok, #state{stimuli = Stimuli, nodes = Nodes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}                        |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}}                                       |
    {noreply, NewState :: #state{}, timeout() | hibernate}                |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}       |
    {stop, Reason :: term(), NewState :: #state{}}.

handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg, State ) -> {noreply, #state{}, timeout ()} | {noreply, #state{}} when
      Msg   :: sim_start | sim_stop | {set_spike_speed, _},
      State :: #state{}.

handle_cast (sim_start, #state{stimuli = Stimuli} = State) ->
    lager:debug ("sim_start"),
    [Stimulus ! sim_start || Stimulus <- Stimuli],

    {noreply, State};

handle_cast (sim_stop, #state{stimuli = Stimuli} = State) ->
    lager:debug ("sim_stop"),
    [Stimulus ! sim_stop || Stimulus <- Stimuli],
    {noreply, State};

handle_cast ({set_spike_speed, Speed}, #state{} = State) ->
    vnn_params:set_spike_speed (Speed),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info (Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info (timeout, #state{}) -> {noreply, #state{}, timeout ()} | {noreply, #state{}}.

handle_info (timeout, State) ->
    %% Val = case Status of true -> 1; false -> 0 end,
    %% vnn_event:send_event (Val),
    %% {noreply, #state{status = not Status}, random:uniform (100) + 50}.
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate (Reason :: (normal | shutdown | {shutdown, term ()} | term ()),
                 State :: term()) -> term().

terminate (_Reason, #state{nodes = Nodes}) ->
    lager:debug ("network terminate"),
    [exit (Node, shutdown) || Node <- Nodes],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: (term () | {down, term ()}), State :: term (), Extra :: term ()) ->
    {ok, #state{}} |
    {error, Reason :: term ()}.

code_change (_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
-spec connect (FromPid :: pid (), ToPid :: pid ()) -> ok.

connect (FromPid, ToPid) ->
    FromPid ! {request_connection_to, ToPid},
    ok.

%%--------------------------------------------------------------------
-spec create_layer (LayerId :: non_neg_integer (), Count :: pos_integer ()) -> [pid ()].

create_layer (LayerId, Count) when Count > 0 ->
    [createNeuron (LayerId) || _ <- lists:seq (1, Count)].

%%--------------------------------------------------------------------
-spec createNeuron (LayerId :: pos_integer ()) -> pid ().

createNeuron (LayerId) ->
    Position = {-140 + vnn_random:uniform (0.0, 280.0),
                 300 - ((LayerId - 1) * 100) + vnn_random:normal (0.0, 10.0),
                -280 + vnn_random:uniform (0.0, 560.0)},
    spawn (vnn_node, create, [neuron, Position]).

%%--------------------------------------------------------------------
-spec createStimulus (Stride, Lines, Row, Col, NodeType) -> pid () when
      Stride   :: non_neg_integer (),
      Lines    :: non_neg_integer (),
      Row      :: non_neg_integer (),
      Col      :: non_neg_integer (),
      NodeType :: boolean ().

createStimulus (Stride, TotalLines, Row, Col, NodeType) ->
    Step = 14,
    Position = {Row * Step - TotalLines * Step / 2 + Step / 2,
                -300.0,
                (Stride - 1 - Col) * Step - Stride * Step / 2 + Step / 2},
    spawn (vnn_node, create, [NodeType, Position]).
