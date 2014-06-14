%% Client messages
-define (RECREATE_NETWORK, 0).
-define (START_SIMULATION, 1).
-define (STOP_SIMULATION,  2).
-define (SET_SPIKE_SPEED,  3).

%% Server messages
-define (SPIKE,       0).
-define (POSITION,    1).
-define (CONNECTION,  2).
-define (NEW_NETWORK, 3).

%% Params
-define (SPIKE_SPEED, 100).
