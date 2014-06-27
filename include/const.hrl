%% Client messages
-define (RECREATE_NETWORK, 0).
-define (START_SIMULATION, 1).
-define (STOP_SIMULATION,  2).
-define (SELECT_NODE,      3).
-define (SET_SPIKE_SPEED,  4).

%% Server messages
-define (SPIKE,              0).
-define (SELECTED_NEIGHBOUR, 1).
-define (SELECTED_INBOUND,   2).
-define (SELECTED_OUTBOUND,  3).
-define (POSITION,           4).
-define (CONNECTION,         5).
-define (NEW_NETWORK,        6).

%% Params
-define (SPIKE_SPEED, 100).

-define (STIMULUS_HELLO_WORLD, 0).

-define (NETWORK_0, 0).
-define (NETWORK_1, 1).
