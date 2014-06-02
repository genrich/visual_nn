%%--------------------------------------------------------------------
%% @doc
%% Main module
%% @end
%%--------------------------------------------------------------------
-module (vnn_network).

-export ([start_link/0, sim_start/0, sim_stop/0]).

-export_type([position/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type position () :: {number (), number (), number ()}.

-record (state, {segments    :: [pid ()],
                 neurons     :: [pid ()],
                 time_factor :: float (),
                 stimuli     :: [pid ()]}).

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
%% @private
%% @doc
%% Initializes the server with new Stimuli and Neurons
%% @end
%%--------------------------------------------------------------------
-spec init ([]) -> {ok, #state{}}.

init ([]) ->
    Stimuli = vnn_stimulus:start (),
    %% Neurons = vnn_segment:start_neurons (),
    %% lists:foldl (fun ({Stimulus, Neuron}, Id) -> connect (Stimulus, Neuron, Id), Id + 1 end,
    %%              0,
    %%              [{Stimulus, Neuron} || Stimulus <- Stimuli, Neuron <- Neurons, random:uniform () =< 0.01]),
    {ok, #state{stimuli = Stimuli}}.

connect (Stimulus, Neuron, Id) ->
    Stimulus ! {connect_outbound, Neuron, Id},
    ok.

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
-spec handle_cast(sim_start | sim_stop, #state{}) -> {noreply, #state{}, timeout ()} | {noreply, #state{}}.

handle_cast (sim_start, #state{stimuli = Stimuli} = State) ->
    lager:debug ("sim_start"),
    [Stimulus ! sim_start || Stimulus <- Stimuli],
    {noreply, State};

handle_cast (sim_stop, #state{stimuli = Stimuli} = State) ->
    lager:debug ("sim_stop"),
    [Stimulus ! sim_stop || Stimulus <- Stimuli],
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

terminate (_Reason, _State) ->
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
%% Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib ("eunit/include/eunit.hrl").

init_test () ->
    {ok, #state{stimuli = Stimuli}} = init ([]),
    ?assert (is_list (Stimuli)),
    ?assert (is_pid (hd (Stimuli))).

connect_test () ->
    {ok, #state{stimuli = _Stimuli}} = init ([]),
    ok.

-endif.
