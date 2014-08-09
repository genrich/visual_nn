#ifndef NETWORK_H
#define NETWORK_H

#include <random>

#define NODE_TYPES       \
    X (stimulus_active), \
    X (stimulus_rest),   \
    X (soma),            \
    X (synapse),         \
    X (dendrite),        \
    X (axon)

enum NodeType
{
    #define X(a) a
    NODE_TYPES
    #undef X
};

class Network
{
    std::random_device rd;
    std::mt19937 rnd {rd ()};

    int id = 0;

    int addNode (int const somaId, NodeType const, float const x, float const y, float const z);
    void createNeuron ();

public:
    std::vector<float>               nodes;
    std::vector<NodeType>            nodeTypes;
    std::vector<int>                 nodeSomaIds;
    std::vector<std::pair<int, int>> connections;

    Network ();
    void init ();
    void createStimulus ();
    void createNetwork ();
};

#endif // NETWORK_H
