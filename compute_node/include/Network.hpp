#ifndef NETWORK_H
#define NETWORK_H

#include <tuple>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/geometry.hpp>

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
namespace bn = boost::numeric::ublas;

using Coord       = float;
using Point       = bg::model::point<Coord, 3, bg::cs::cartesian>;
using BoundingBox = bg::model::box<Point>;
using Matrix      = bn::matrix<Coord>;
using NeuronCount = int;
using ActivityMap = std::vector<std::vector<int>>;

struct NeuronNode
{
    int      somaId;
    NodeType type;
    Point    point;
};

extern ActivityMap const stimulusActivityMap;

class StimulusLayer
{
public:
    std::vector<NeuronNode>          nodes;
    std::vector<std::pair<int, int>> connections;

    StimulusLayer (BoundingBox const& box, ActivityMap const& activityMap);
};

class UniformLayer
{
    void createNeuron (Point const& center);
public:
    std::vector<NeuronNode>          nodes;
    std::vector<std::pair<int, int>> connections;

    UniformLayer (BoundingBox const& box, int const neuronCount);
};

class Network
{
public:
    std::vector<NeuronNode>          nodes;
    std::vector<std::pair<int, int>> connections;

    template <typename... Args> explicit Network (Args&&... args) { [] (auto...) {} ((add (std::forward<Args> (args)), 1)...); }
    void add (StimulusLayer const& layer);
    void add (UniformLayer const& layer);
};

struct NodeInfo
{
    int   isNotProcessed;
    int   nearestNode;
    Coord pathTroughNearest;
};

std::pair<int, int>
findNearest (Matrix const&         distances,
            std::vector<NodeInfo>& nodeInfos,
            int const              node,
            Coord const            factor);
void
initDistances (Matrix& distances, std::vector<NeuronNode> const& nodes);

#endif // NETWORK_H
