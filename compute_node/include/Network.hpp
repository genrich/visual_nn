#ifndef NETWORK_H
#define NETWORK_H

#include <tuple>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/geometry.hpp>

#define NODE_TYPES       \
    X (dendrite),        \
    X (axon),            \
    X (soma),            \
    X (stimulus_active), \
    X (stimulus_rest)

enum class NodeType
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
using NeuronCount = size_t;
using ActivityMap = std::vector<std::vector<int>>;

struct NeuronNode
{
    Point    point;
    size_t   somaId;
    NodeType type;
};

extern ActivityMap const stimulusActivityMap;

class StimulusLayer
{
public:
    std::vector<NeuronNode>                nodes;
    std::vector<std::pair<size_t, size_t>> connections;

    StimulusLayer (BoundingBox const& box, ActivityMap const& activityMap);
};

class UniformLayer
{
    struct ArborParams
    {
        size_t   nodesCount;
        NodeType nodeType;
        Coord    std;
        Point    bias;
    };

    void createNeuronAt       (Coord const x, Coord const y, Coord const z, int const feedDirection);
    void createDendriticArbor (size_t const neuronId, Point const origin);
    void createAxonalArbor    (size_t const neuronId, Point const origin, int const feedDirection);
public:
    std::vector<NeuronNode>                nodes;
    std::vector<std::pair<size_t, size_t>> connections;

    UniformLayer (BoundingBox const& box, int const neuronCount, int const feedDirection);
};

class TestLayer
{
public:
    std::vector<NeuronNode>                nodes;
    std::vector<std::pair<size_t, size_t>> connections;

    TestLayer (std::initializer_list<NeuronNode> const nodes);
};

class Network
{
    void connect ();
    void trim ();
public:
    std::vector<NeuronNode>                nodes;
    std::vector<std::pair<size_t, size_t>> connections;

	template <class BBOX>
	bool kdtree_get_bbox (BBOX &bb) const
    {
        return false;
    }

    template <typename... Args>
    explicit Network (Args&&... args)
    {
        [] (auto...) {} ((add (std::forward<Args> (args)), 1)...);
        connect ();
        trim ();
    }
    void add (StimulusLayer const& layer);
    void add (UniformLayer const& layer);
    void add (TestLayer const& layer);
	inline auto kdtree_get_point_count () const -> size_t;
	inline auto kdtree_distance (Coord const * const p1, size_t const idx_p2, size_t size) const -> Coord;
	inline auto kdtree_get_pt (size_t const idx, int dim) const -> Coord;
};

struct NodeInfo
{
    int    isNotProcessed;
    size_t nearestNode;
    Coord  pathTroughNearest;
};

auto findNearest (Matrix const& distances, std::vector<NodeInfo>& nodeInfos, size_t const node, Coord const factor)
    -> std::pair<size_t, size_t>;
void initDistances (Matrix& distances, std::vector<NeuronNode> const& nodes);

#endif // NETWORK_H
