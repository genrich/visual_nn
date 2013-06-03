-module (vnn_network).

-export ([start_link/0, sim_start/0, sim_stop/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {status :: boolean ()}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link () -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Tmp
%%
%% @spec start_link () -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
sim_start () ->
    gen_server:cast (?MODULE, sim_start).

%%--------------------------------------------------------------------
%% @doc
%% Tmp
%%
%% @spec start_link () -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
sim_stop () ->
    gen_server:cast (?MODULE, sim_stop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init (Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init ([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call (Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast (Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast (sim_start, _State) ->
    lager:debug ("sim_start"),
    {noreply, #state{status = false}, 1000};

handle_cast (sim_stop, _State) ->
    lager:debug ("sim_stop"),
    {noreply, #state{}}.

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
handle_info (timeout, #state{status = Status}) ->
    Val = case Status of true -> 1; false -> 0 end,
    vnn_event:send_event (Val),
    {noreply, #state{status = not Status}, random:uniform (100) + 50}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate (Reason, State) -> void ()
%% @end
%%--------------------------------------------------------------------
terminate (_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change (OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change (_OldVsn, State, _Extra) ->
    {ok, State}.
