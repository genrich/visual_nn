%%--------------------------------------------------------------------
%% @doc
%% Event manager
%% @end
%%--------------------------------------------------------------------
-module (vnn_event).

-export ([start_link/0,
          add_handler/2,
          delete_handler/2,
          send_event/1,
          send_stimulus_pos/2,
          send_soma_pos/2,
          send_connection/3,
          send_stimulus_spike/2]).

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
-spec send_event (Arg :: term ()) -> ok.

send_event (Arg) ->
    gen_event:notify (?MODULE, {send_event, Arg}).

%%--------------------------------------------------------------------
%% @doc
%% Send stimulus position
%% @end
%%--------------------------------------------------------------------
-spec send_stimulus_pos (Id :: non_neg_integer (), Pos :: vnn_network:position ()) -> ok.

send_stimulus_pos (Id, Pos) ->
    gen_event:notify (?MODULE, {send_stimulus_pos, Id, Pos}).

%%--------------------------------------------------------------------
%% @doc
%% Send stimulus spike
%% @end
%%--------------------------------------------------------------------
-spec send_stimulus_spike (StimulusId, Connections) -> ok when
    StimulusId :: non_neg_integer (),
    Connections :: {ConnectionsCount :: non_neg_integer (), Connections :: []}.

send_stimulus_spike (Id, Connections) ->
    gen_event:notify (?MODULE, {send_stimulus_spike, Id, Connections}).

send_soma_pos (Id, Pos) ->
    gen_event:notify (?MODULE, {send_soma_pos, Id, Pos}).

send_connection (ConnId, FromPosition, ToPosition) ->
    gen_event:notify (?MODULE, {send_connection, ConnId, FromPosition, ToPosition}).
