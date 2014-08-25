#ifndef NETWORK_H
#define NETWORK_H

#include <random>
#include <tuple>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/geometry/geometry.hpp>

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

namespace bg = boost::geometry;
namespace bu = boost::numeric::ublas;

using coord_type = float;
using Point      = bg::model::point<coord_type, 3, bg::cs::cartesian>;
using Matrix     = bu::matrix<coord_type>;

struct NeuronNode
{
    int      somaId;
    NodeType type;
    Point    point;
};

class Network
{
    std::random_device rd;
    std::mt19937       rnd {rd ()};

    void createNeuron ();

public:
    std::vector<NeuronNode>          nodes;
    std::vector<std::pair<int, int>> connections;

    Network ();
    void init ();
    void createStimulus ();
    void createNetwork ();
};

struct NodeInfo
{
    int        isNotProcessed;
    int        nearestNode;
    coord_type pathTroughNearest;
};

std::pair<int, int>
findNearest (Matrix const&         distances,
            std::vector<NodeInfo>& nodeInfos,
            int const              node,
            coord_type const       factor);
void
initDistances (Matrix& distances, std::vector<NeuronNode> const& nodes);

#endif // NETWORK_H
