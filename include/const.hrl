%% Client messages
-define (MSG_RECREATE_NETWORK, 0).
-define (MSG_START_SIMULATION, 1).
-define (MSG_STOP_SIMULATION,  2).
-define (MSG_SELECT_NODE,      3).
-define (MSG_DESELECT_NODE,    4).
-define (MSG_SET_SLOWDOWN,     5).

%% Server messages
-define (MSG_SPIKE,              0).
-define (MSG_SELECTED_NEIGHBOUR, 1).
-define (MSG_SELECTED_INBOUND,   2).
-define (MSG_SELECTED_OUTBOUND,  3).
-define (MSG_POSITION,           4).
-define (MSG_CONNECTION,         5).
-define (MSG_NEW_NETWORK,        6).

%% Node types
-define (NODE_SOMA,     0).
-define (NODE_SYNAPSE,  1).
-define (NODE_DENDRITE, 2).
-define (NODE_AXON,     3).

%% Params
-define (PARAM_SLOWDOWN,            10000.0).    % times slower
-define (PARAM_SPIKE_SPEED,         25000000.0). % microns/sec
-define (PARAM_ACTIVE_RATE,         700.0).      % spikes/sec
-define (PARAM_REST_RATE,           10.0).       % spikes/sec
-define (PARAM_ABSOLUTE_REFRACTORY, 0.001).      % sec

-define (STIMULUS_HELLO_WORLD, 0).

-define (NETWORK_0, 0).
-define (NETWORK_1, 1).
-define (NETWORK_2, 2).
-define (NETWORK_3, 3).
