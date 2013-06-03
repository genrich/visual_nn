-module (vnn_sup).

-export ([start_link/0]).

-behaviour (supervisor).
-export ([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    RestartStrategy = {one_for_one, 3, 60},
    Network = {vnn_network, {vnn_network, start_link, []},
               permanent, 2000, worker, [vnn_network]},
    EventManager = {vnn_event, {vnn_event, start_link, []},
                    permanent, 2000, worker, [vnn_event]},
    EmbeddedYaws = {vnn_yaws_sup, {vnn_yaws_sup, start_link, []},
                    permanent, 2000, supervisor, [vnn_yaws_sup]},
    {ok, {RestartStrategy, [Network, EventManager, EmbeddedYaws]}}.
