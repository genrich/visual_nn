%%--------------------------------------------------------------------------------------------------
%% @doc
%% Compute node delegation module
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_cnode).

-export ([start_link/0, create_network/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (s, {compute_node :: pid ()}).

-include_lib ("lager/include/lager.hrl").

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_link () -> {ok, pid ()} | ignore | {error,  {already_started, pid ()} | term ()}.
%%--------------------------------------------------------------------------------------------------
start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Delegate network creation to the compute node
%% @end
%%--------------------------------------------------------------------------------------------------
-spec create_network () -> ok.
%%--------------------------------------------------------------------------------------------------
create_network () ->
    gen_server:cast (?MODULE, create_network).


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server with new Stimuli and Neurons
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init ([]) -> {ok, #s{}}.
%%--------------------------------------------------------------------------------------------------
init ([]) ->
    process_flag (trap_exit, true),
    {ok, #s{}}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_call (_, _, #s{}) ->
    {reply, ok, NewState :: #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_call (_, _, #s{} = State) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_cast (_, #s{}) -> {noreply, #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_cast (create_network, #s{compute_node = CNPid} = State) when CNPid =/= undefined ->
    lager:debug ("create_network"),
    CNPid ! create_network,
    {noreply, State};

handle_cast (_, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------------------------------------
-spec handle_info (_, #s{}) -> {noreply, #s{}}.
%%--------------------------------------------------------------------------------------------------
handle_info ({compute_node_started, Pid}, State) ->
    lager:debug ("compute_node_started ~p", [Pid]),
    link (Pid),
    {noreply, State#s{compute_node = Pid}};

handle_info ({'EXIT', Pid, Reason}, State) when Pid =:= State#s.compute_node ->
    lager:error ("compute_node unlinked Pid=~p Reason=~p", [Pid, Reason]),
    {noreply, State#s{compute_node = undefined}};

handle_info ({add_node, {Id, Type, Pos}}, State) ->
    vnn_network:add_node (Id, Type, Pos),
    {noreply, State};

handle_info (_, State) ->
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
terminate (_Reason, _State) ->
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
