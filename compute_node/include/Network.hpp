#ifndef NETWORK_H
#define NETWORK_H

#include <random>

#define NODE_TYPES       \
    X (stimulus_active), \
    X (stimulus_rest),   \
    X (neuron),          \
    X (node)

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
    void addNode (NodeType const, float const x, float const y, float const z);
    void createNeuron ();

public:
    std::vector<float>    nodes;
    std::vector<NodeType> nodeTypes;

    Network ();
    void createStimulus ();
    void createNetwork ();
};

#endif // NETWORK_H
