#include <random>

#include "nanoflann.hpp"

#include "Network.hpp"

using namespace std;
using namespace nanoflann;
using KDTree = KDTreeSingleIndexAdaptor<L2_Simple_Adaptor<Coord, Network>, Network, 3>;

static random_device rd;
static mt19937       rnd {rd ()};

ActivityMap const stimulusActivityMap
{
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,1,1,1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0},
    {0,0,0,0,1,1,0,1,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0},
    {0,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
};

//--------------------------------------------------------------------------------------------------
void transform (vector<NeuronNode> const&  nodesFrom,
                vector<NeuronNode>&        nodesTo,
                vector<pair<size_t, size_t>> const& connectionsFrom,
                vector<pair<size_t, size_t>>&       connectionsTo)
{
    auto const base = nodesTo.size ();

    transform (nodesFrom.cbegin (), nodesFrom.cend (), back_inserter (nodesTo), [base] (auto const& node)
    {
        return NeuronNode {node.point, base + node.somaId, node.type};
    });

    transform (connectionsFrom.cbegin (), connectionsFrom.cend (), back_inserter (connectionsTo), [base] (auto const& edge)
    {
        return make_pair (base + edge.first, base + edge.second);
    });
}

//--------------------------------------------------------------------------------------------------
void initDistances (Matrix& distances, std::vector<NeuronNode> const& nodes)
{
    for (size_t row = 0; row < distances.size1 (); ++row)
    {
        for (size_t col = 0; col < row; ++col)
        {
            auto const d = bg::distance (nodes[row].point, nodes[col].point);
            distances (row, col) = d;
            distances (col, row) = d;
        }
        distances (row, row) = 0;
    }
}

//--------------------------------------------------------------------------------------------------
auto findNearest (Matrix const& distances, std::vector<NodeInfo>& nodeInfos, size_t const node, Coord const factor)
    -> pair<size_t, size_t>
{
    int        minIndex       = 0;
    Coord      minDistance    = numeric_limits<Coord>::max ();
    auto const nodePathLength = nodeInfos[node].pathTroughNearest;

    for (int i = 0; i < nodeInfos.size (); ++i)
    {
        if (nodeInfos[i].isNotProcessed)
        {
            auto const d = factor * distances (node, i) + nodePathLength;

            if (d < nodeInfos[i].pathTroughNearest)
            {
                nodeInfos[i].nearestNode       = node;
                nodeInfos[i].pathTroughNearest = distances (node, i) + nodePathLength;

                if (d < minDistance)
                {
                    minDistance = d;
                    minIndex    = i;
                }
            }
            else
            {
                if (nodeInfos[i].pathTroughNearest < minDistance)
                {
                    minDistance = nodeInfos[i].pathTroughNearest;
                    minIndex    = i;
                }
            }
        }
    }

    nodeInfos[minIndex].isNotProcessed = 0;
    return make_pair (nodeInfos[minIndex].nearestNode, minIndex);
}

//--------------------------------------------------------------------------------------------------
void Network::add (StimulusLayer const& layer)
{
    transform (layer.nodes, nodes, layer.connections, connections);
}

//--------------------------------------------------------------------------------------------------
void Network::add (UniformLayer const& layer)
{
    transform (layer.nodes, nodes, layer.connections, connections);
}

//--------------------------------------------------------------------------------------------------
void Network::add (TestLayer const& layer)
{
    transform (layer.nodes, nodes, layer.connections, connections);
}

//--------------------------------------------------------------------------------------------------
inline auto Network::kdtree_get_point_count () const
-> size_t
{
    return nodes.size ();
}

//--------------------------------------------------------------------------------------------------
inline auto Network::kdtree_distance (Coord const * const p1, size_t const idx_p2, size_t size) const
-> Coord
{
    Coord d0 = p1[0] - bg::get<0> (nodes[idx_p2].point);
    Coord d1 = p1[1] - bg::get<1> (nodes[idx_p2].point);
    Coord d2 = p1[2] - bg::get<2> (nodes[idx_p2].point);
    return d0*d0 + d1*d1 + d2*d2;
}

//--------------------------------------------------------------------------------------------------
inline auto Network::kdtree_get_pt (size_t const idx, int dim) const
-> Coord
{
    if      (dim==0) return bg::get<0> (nodes[idx].point);
    else if (dim==1) return bg::get<1> (nodes[idx].point);
    else             return bg::get<2> (nodes[idx].point);
}

//--------------------------------------------------------------------------------------------------
void Network::connect ()
{
    Coord constexpr radius = 250;

	KDTree index {3, *this, KDTreeSingleIndexAdaptorParams {10}};
	index.buildIndex ();

    vector<pair<size_t, Coord>> result;

    for (int i = 0; i < nodes.size (); ++i)
    {
        if (nodes[i].type == NodeType::stimulus_active || nodes[i].type == NodeType::stimulus_rest)
        {
            auto const nMatches = index.radiusSearch (reinterpret_cast<Coord*> (&nodes[i].point), radius, result, SearchParams {});
            for (int j = 0; j < nMatches; ++j)
            {
                if (nodes[result[j].first].type == NodeType::dendrite)
                    connections.push_back ({i, result[j].first});
            }
            result.clear ();
        }
    }
}

//--------------------------------------------------------------------------------------------------
StimulusLayer::StimulusLayer (BoundingBox const& box, ActivityMap const& activityMap)
{
    Coord const minX = bg::get<bg::min_corner, 0> (box),
                minZ = bg::get<bg::min_corner, 2> (box),

                maxX = bg::get<bg::max_corner, 0> (box),
                maxZ = bg::get<bg::max_corner, 2> (box),

                y = bg::get<bg::min_corner, 1> (box);

    Coord const stepX = (maxX - minX) / static_cast<Coord> (activityMap.size ()),
                stepZ = (maxZ - minZ) / static_cast<Coord> (activityMap.at (0).size ());

    for (int i = 0; i < activityMap.size (); ++i)
        for (int j = 0; j < activityMap[i].size (); ++j)
        {
            auto const somaId = nodes.size ();
            if (activityMap[i][j])
                nodes.push_back ({Point {minX + i * stepX, y, minZ + j * stepZ}, somaId, NodeType::stimulus_active});
            else
                nodes.push_back ({Point {minX + i * stepX, y, minZ + j * stepZ}, somaId, NodeType::stimulus_rest});
        }
}

//--------------------------------------------------------------------------------------------------
UniformLayer::UniformLayer (BoundingBox const& box, int const neuronCount)
{
    Coord const minX = bg::get<bg::min_corner, 0> (box),
                minY = bg::get<bg::min_corner, 1> (box),
                minZ = bg::get<bg::min_corner, 2> (box),

                maxX = bg::get<bg::max_corner, 0> (box),
                maxY = bg::get<bg::max_corner, 1> (box),
                maxZ = bg::get<bg::max_corner, 2> (box);

    uniform_real_distribution<Coord> uniformX {minX, maxX}, uniformY {minY, maxY}, uniformZ {minZ, maxZ};

    for (int i = 0; i < neuronCount; ++i)
        createNeuronAt (uniformX (rnd), uniformY (rnd), uniformZ (rnd));
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createNeuronAt (Coord const x, Coord const y, Coord const z)
{
    auto const neuronId = nodes.size ();
    auto const origin   = Point {x, y, z};

    nodes.push_back ({Point {x, y, z}, neuronId, NodeType::soma});

    createArbor (neuronId, origin, dendriticParams);
    // createArbor (neuronId, origin, axonalParams);
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createArbor (size_t const neuronId, Point const origin, ArborParams const& params)
{
    size_t constexpr somaId = 0;

    normal_distribution<Coord> normalX {bg::get<0> (origin), params.std},
                               normalZ {bg::get<2> (origin), params.std};
    lognormal_distribution<Coord> logNormal {0.0, 0.5};

    auto const nodesCountAndSoma = params.nodesCount + 1;

    vector<NeuronNode> arborNodes;
    arborNodes.reserve (nodesCountAndSoma);
    arborNodes.push_back ({origin, somaId, NodeType::soma});

    for (int i = 0; i < params.nodesCount; ++i)
        arborNodes.push_back ({Point {normalX (rnd), bg::get<1> (origin) - 100.0f + 100.0f * logNormal (rnd), normalZ (rnd)},
                               somaId, NodeType::dendrite});

    Matrix distances {nodesCountAndSoma, nodesCountAndSoma};
    initDistances (distances, arborNodes);

    std::vector<NodeInfo> nodeInfos (nodesCountAndSoma, {1, somaId, numeric_limits<Coord>::max ()});
    nodeInfos[somaId].isNotProcessed    = 0;
    nodeInfos[somaId].pathTroughNearest = 0;

    size_t node = somaId, nearest;
    vector<pair<size_t, size_t>> arborConnections;
    arborConnections.reserve (params.nodesCount);
    for (size_t i = 0; i < params.nodesCount; ++i)
    {
        tie (nearest, node) = findNearest (distances, nodeInfos, node, 0.85);
        arborConnections.push_back ({somaId + node, somaId + nearest});
    }

    // move arbor nodes/connections to the network
    auto const base = nodes.size ();
    auto rebase = [base, neuronId] (size_t const id) { return id == somaId ? neuronId : base + id - 1; };
    transform (arborNodes.cbegin () + 1/*skip soma*/, arborNodes.cend (), back_inserter (nodes), [&rebase] (auto const& node)
    {
        return NeuronNode {node.point, rebase (node.somaId), node.type};
    });

    transform (arborConnections.cbegin (), arborConnections.cend (), back_inserter (connections), [&rebase] (auto const& edge)
    {
        return make_pair (rebase (edge.first), rebase (edge.second));
    });
}

//--------------------------------------------------------------------------------------------------
TestLayer::TestLayer (initializer_list<NeuronNode> const testNodes)
{
    for (auto const& node : testNodes)
        nodes.push_back (node);
}
