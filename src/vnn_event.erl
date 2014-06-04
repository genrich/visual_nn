%%--------------------------------------------------------------------
%% @doc
%% Event manager
%% @end
%%--------------------------------------------------------------------
-module (vnn_event).

-export ([start_link/0,
          add_handler/2,
          delete_handler/2,
          notify_position/2,
          notify_connection/2,
          notify_spike/1]).

%%--------------------------------------------------------------------
%% @doc
%% Start event manager
%% @end
%%--------------------------------------------------------------------
-spec start_link () -> {ok, pid ()} | {error, {already_started, pid ()}}.

start_link () ->
    gen_event:start_link ({local, ?MODULE}).

%%--------------------------------------------------------------------
%% @doc
%% Adds new event handler
%% @end
%%--------------------------------------------------------------------
-spec add_handler (Handler :: atom (), Args :: []) -> ok | {'EXIT', term ()} | term ().

add_handler (Handler, Args) ->
    gen_event:add_handler (?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Delete event handler
%% @end
%%--------------------------------------------------------------------
-spec delete_handler (Handler :: atom (), Args :: []) -> term () | {error, module_not_found} | {'EXIT', term ()}.

delete_handler (Handler, Args) ->
    gen_event:delete_handler (?MODULE, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Send event to be processed by event handlers
%% @end
%%--------------------------------------------------------------------
%% -spec send_event (Arg :: term ()) -> ok.
%%
%% send_event (Arg) ->
%%     gen_event:notify (?MODULE, {send_event, Arg}).

%%--------------------------------------------------------------------
%% @doc
%% Notify node position
%% @end
%%--------------------------------------------------------------------
-spec notify_position (Id, Position) -> ok when
    Id       :: non_neg_integer (),
    Position :: vnn_network:position ().

notify_position (Id, Position) ->
    gen_event:notify (?MODULE, {notify_position, Id, Position}).

%%--------------------------------------------------------------------
%% @doc
%% Notify nodes connection
%% @end
%%--------------------------------------------------------------------
-spec notify_connection (FromId, ToId) -> ok when
    FromId :: non_neg_integer (),
    ToId   :: non_neg_integer ().

notify_connection (FromId, ToId) ->
    gen_event:notify (?MODULE, {notify_connection, FromId, ToId}).

%%--------------------------------------------------------------------
%% @doc
%% Notify node spike
%% @end
%%--------------------------------------------------------------------
-spec notify_spike (Id) -> ok when
      Id :: non_neg_integer ().

notify_spike (Id) ->
    gen_event:notify (?MODULE, {notify_spike, Id}).
