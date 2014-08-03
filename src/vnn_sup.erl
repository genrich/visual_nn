%%--------------------------------------------------------------------------------------------------
%% @doc
%% Root application supervisor
%% @end
%%--------------------------------------------------------------------------------------------------
-module (vnn_sup).

-export ([start_link/0, start_network/1, stop_network/0]).

-behaviour (supervisor).
-export ([init/1]).

-define (SERVER, ?MODULE).

%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start root supervisor
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_link () -> {ok, pid ()} | ignore | {error, {already_started, pid ()} | {shutdown, term ()} | term ()}.
%%--------------------------------------------------------------------------------------------------
start_link () ->
    supervisor:start_link ({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Start neural network
%% @end
%%--------------------------------------------------------------------------------------------------
-spec start_network (NetworkId :: non_neg_integer ()) -> {ok, pid ()}.
%%--------------------------------------------------------------------------------------------------
start_network (NetworkId) ->
    Network = {vnn_network, {vnn_network, start_link, [NetworkId]},
               temporary, 2000, worker, [vnn_network]},
    supervisor:start_child (?SERVER, Network).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Stop neural network
%% @end
%%--------------------------------------------------------------------------------------------------
-spec stop_network () -> ok.
%%--------------------------------------------------------------------------------------------------
stop_network () ->
    supervisor:terminate_child (?SERVER, vnn_network).


%%--------------------------------------------------------------------------------------------------
%% @doc
%% Initialize supervision tree
%% @end
%%--------------------------------------------------------------------------------------------------
-spec init ([]) -> {ok, {{one_for_one, non_neg_integer (), non_neg_integer ()},
                         [{atom (), {atom (), atom (), []}, permanent, non_neg_integer (), worker | supervisor, [atom ()]}]}}.
%%--------------------------------------------------------------------------------------------------
init ([]) ->
    RestartStrategy = {one_for_one, 3, 60},
    ComputeNode     = {vnn_cnode,    {vnn_cnode,    start_link, []}, permanent, 2000, worker,     [vnn_cnode]},
    Utils           = {vnn_utils,    {vnn_utils,    start_link, []}, permanent, 2000, worker,     [vnn_utils]},
    EventManager    = {vnn_event,    {vnn_event,    start_link, []}, permanent, 2000, worker,     [vnn_event]},
    EmbeddedYaws    = {vnn_yaws_sup, {vnn_yaws_sup, start_link, []}, permanent, 2000, supervisor, [vnn_yaws_sup]},
    {ok, {RestartStrategy, [ComputeNode, Utils, EventManager, EmbeddedYaws]}}.
