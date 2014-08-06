var CONST =
{
    // Client messages
    MSG_RECREATE_NETWORK: 0,
    MSG_START_SIMULATION: 1,
    MSG_STOP_SIMULATION: 2,
    MSG_SELECT_NODE: 3,
    MSG_SET_SLOWDOWN: 4,

    // Server messages
    MSG_SPIKE: 0,
    MSG_SELECTED_NEIGHBOUR: 1,
    MSG_SELECTED_INBOUND: 2,
    MSG_SELECTED_OUTBOUND: 3,
    MSG_POSITION: 4,
    MSG_CONNECTION: 5,
    MSG_NEW_NETWORK: 6,

    // Node types
    NODE_NODE: 0,
    NODE_SYNAPSE: 1,
    NODE_NEURON: 2,

    // Params
    PARAM_SLOWDOWN: 10000.0,
    PARAM_SPIKE_SPEED: 25000000.0,
    PARAM_ACTIVE_RATE: 700.0,
    PARAM_REST_RATE: 10.0,
    PARAM_ABSOLUTE_REFRACTORY: 0.001,

    STIMULUS_HELLO_WORLD: 0,

    NETWORK_0: 0,
    NETWORK_1: 1,
    NETWORK_2: 2,
    NETWORK_3: 3,
}
