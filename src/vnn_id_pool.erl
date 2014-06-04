%%--------------------------------------------------------------------
%% @doc
%% Main module
%% @end
%%--------------------------------------------------------------------
-module (vnn_id_pool).

-export ([start_link/0, id/0]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {last_id = 0 :: non_neg_integer ()}).

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
%% Generate new id
%% @end
%%--------------------------------------------------------------------
-spec id () -> Id :: non_neg_integer ().

id () ->
    gen_server:call (?MODULE, id).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server with new Stimuli and Neurons
%% @end
%%--------------------------------------------------------------------
-spec init ([]) -> {ok, #state{}}.

init ([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(id, _From, State :: #state{}) ->
    {reply, NewId :: non_neg_integer (), NewState :: #state{}}.

handle_call (id, _From, #state{last_id = Id} = State) ->
    {reply, Id, State#state{last_id = Id + 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(_, #state{}) -> {noreply, #state{}}.

handle_cast (_, State) ->
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
-spec handle_info (_, #state{}) -> {noreply, #state{}}.

handle_info (_, State) ->
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
