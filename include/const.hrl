%% Client messages
-define (RECREATE_NETWORK, 0).
-define (START_SIMULATION, 1).
-define (STOP_SIMULATION,  2).
-define (SELECT_NODE,      3).
-define (SET_SPIKE_SPEED,  4).

%% Server messages
-define (SPIKE,       0).
-define (POSITION,    1).
-define (CONNECTION,  2).
-define (NEW_NETWORK, 3).

%% Params
-define (MAX_ID, 16777215).

-define (SPIKE_SPEED, 100).

-define (STIMULUS_HELLO_WORLD, 0).

-define (NETWORK_0, 0).
-define (NETWORK_1, 1).
