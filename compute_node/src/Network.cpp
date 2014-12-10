#include <random>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/breadth_first_search.hpp>

#include "nanoflann.hpp"

#include "Network.hpp"

using namespace std;
using namespace nanoflann;
using namespace boost;

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

    for (size_t i = 0; i < nodeInfos.size (); ++i)
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
    Coord constexpr radius = 30 * 30;

    KDTreeSingleIndexAdaptor<L2_Simple_Adaptor<Coord, Network>, Network, 3>
        index {3, *this, KDTreeSingleIndexAdaptorParams {10}};
	index.buildIndex ();

    vector<pair<size_t, Coord>> result;
    uniform_int_distribution<> flip {0, 1};

    for (int i = 0; i < nodes.size (); ++i)
    {
        if (nodes[i].type == NodeType::stimulus_active || nodes[i].type == NodeType::stimulus_rest)
        {
            auto const nMatches = index.radiusSearch (reinterpret_cast<Coord*> (&nodes[i].point), radius, result, SearchParams {});
            for (int j = 0; j < nMatches; ++j)
            {
                if (nodes[result[j].first].type == NodeType::dendrite && flip (rnd) == 0)
                    connections.push_back ({i, result[j].first});
            }
            result.clear ();
        }
        else if (nodes[i].type == NodeType::axon)
        {
            auto const nMatches = index.radiusSearch (reinterpret_cast<Coord*> (&nodes[i].point), radius, result, SearchParams {});
            for (int j = 0; j < nMatches; ++j)
            {
                if (nodes[result[j].first].type == NodeType::dendrite
                        && nodes[result[j].first].somaId != nodes[i].somaId
                        && flip (rnd) == 0)
                    connections.push_back ({i, result[j].first});
            }
            result.clear ();
        }
    }
}

//--------------------------------------------------------------------------------------------------
void Network::trim ()
{
    using Graph = adjacency_list<vecS, vecS, bidirectionalS, NeuronNode>;
    using VertexDescriptor = graph_traits<Graph>::vertex_descriptor;
    using VertexIterator   = graph_traits<Graph>::vertex_iterator;
    using EdgeIterator     = graph_traits<Graph>::edge_iterator;

    VertexIterator vi, vi_end;
    EdgeIterator   ei, ei_end;

    // init graph
    Graph g {connections.cbegin (), connections.cend (), nodes.size ()};
    for (tie (vi, vi_end) = vertices (g); vi != vi_end; ++vi)
    {
        g[*vi] = nodes[*vi];
    }
    nodes.clear ();
    connections.clear ();

    // clear axonal dead ends
    bool wasTrimmed;
    do
    {
        wasTrimmed = false;
        for (tie (vi, vi_end) = vertices (g); vi != vi_end; ++vi)
        {
            if (g[*vi].type == NodeType::axon && in_degree (*vi, g) == 1 && out_degree (*vi, g) == 0)
            {
                clear_vertex (*vi, g);
                wasTrimmed = true;
            }
        }
    } while (wasTrimmed);

    // find all reachable from stimulus
    vector<uint> colors (num_vertices (g), white_color);
    auto colorMap = color_map (&colors[0]);
    for (tie (vi, vi_end) = vertices (g); vi != vi_end; ++vi)
    {
        auto const v = g[*vi];
        if (v.type == NodeType::stimulus_active || v.type == NodeType::stimulus_rest)
        {
            breadth_first_visit (g, *vi, colorMap);
        }
    }

    // assign reachable new indexes and push to the network
    size_t nodeIdx = 1; // 0 means not reachable
    for (size_t i = 0; i < colors.size (); ++i)
    {
        if (colors[i] == white_color)
        {
            colors[i] = 0;
        }
        else
        {
            colors[i] = nodeIdx++;

            auto const v = g[vertex (i, g)];
            assert (v.somaId <= i);
            assert (colors[v.somaId] != 0);
            nodes.push_back ({v.point, colors[v.somaId] - 1, v.type});
        }
    }
    assert (nodes.size () == nodeIdx - 1);

    // push reachable edges to the network
    for (tie (ei, ei_end) = edges (g); ei != ei_end; ++ei)
    {
        auto const u = source (*ei, g), v = target (*ei, g);
        if (colors[u] != 0 && colors[v] != 0)
        {
            connections.push_back ({colors[u] - 1, colors[v] - 1});
            assert (colors[u] - 1 < nodeIdx - 1);
            assert (colors[v] - 1 < nodeIdx - 1);
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
UniformLayer::UniformLayer (BoundingBox const& box, int const neuronCount, int const feedDirection)
{
    Coord const minX = bg::get<bg::min_corner, 0> (box),
                minY = bg::get<bg::min_corner, 1> (box),
                minZ = bg::get<bg::min_corner, 2> (box),

                maxX = bg::get<bg::max_corner, 0> (box),
                maxY = bg::get<bg::max_corner, 1> (box),
                maxZ = bg::get<bg::max_corner, 2> (box);

    uniform_real_distribution<Coord> uniformX {minX, maxX}, uniformY {minY, maxY}, uniformZ {minZ, maxZ};

    for (int i = 0; i < neuronCount; ++i)
        createNeuronAt (uniformX (rnd), uniformY (rnd), uniformZ (rnd), feedDirection);
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createNeuronAt (Coord const x, Coord const y, Coord const z, int const feedDirection)
{
    auto const neuronId = nodes.size ();
    auto const origin   = Point {x, y, z};

    nodes.push_back ({origin, neuronId, NodeType::soma});

    createDendriticArbor (neuronId, origin);
    createAxonalArbor (neuronId, origin, feedDirection);
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createDendriticArbor (size_t const neuronId, Point const origin)
{
    auto constexpr std = 50;
    auto constexpr nodesCount = 100;
    auto constexpr nodesCountAndSoma = nodesCount + 1;

    normal_distribution<Coord> normalX {bg::get<0> (origin), std},
                               normalY {bg::get<1> (origin), std},
                               normalZ {bg::get<2> (origin), std};

    vector<NeuronNode> arborNodes;
    arborNodes.reserve (nodesCountAndSoma);
    arborNodes.push_back ({origin, 0, NodeType::soma});

    for (int i = 0; i < nodesCount; ++i)
        arborNodes.push_back ({Point {normalX (rnd), normalY (rnd), normalZ (rnd)}, 0, NodeType::dendrite});

    Matrix distances {nodesCountAndSoma, nodesCountAndSoma};
    initDistances (distances, arborNodes);

    std::vector<NodeInfo> nodeInfos (nodesCountAndSoma, {1, 0, numeric_limits<Coord>::max ()});
    nodeInfos[0].isNotProcessed    = 0;
    nodeInfos[0].pathTroughNearest = 0;

    size_t node = 0, nearest;
    vector<pair<size_t, size_t>> arborConnections;
    arborConnections.reserve (nodesCount);
    for (size_t i = 0; i < nodesCount; ++i)
    {
        tie (nearest, node) = findNearest (distances, nodeInfos, node, 0.85);
        arborConnections.push_back ({node, nearest});
    }

    // move arbor nodes/connections to the network
    auto const base = nodes.size ();
    auto rebase = [base, neuronId] (size_t const id) { return id == 0 ? neuronId : base + id - 1; };
    transform (arborNodes.cbegin () + 1/*skip soma*/, arborNodes.cend (), back_inserter (nodes), [&rebase] (auto const& node)
    {
        return NeuronNode {node.point, rebase (node.somaId), node.type};
    });

    transform (arborConnections.cbegin (), arborConnections.cend (), back_inserter (connections), [&] (auto const& edge)
    {
        return make_pair (rebase (edge.first), rebase (edge.second));
    });
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createAxonalArbor (size_t const neuronId, Point const origin, int const f)
{
    Coord constexpr stdXZ = 50, stdY = 20;
    size_t constexpr nodesCount = 100;
    Coord constexpr layerDist = 200;

    normal_distribution<Coord> normalX {bg::get<0> (origin), stdXZ},
                               normalY {0, stdY},
                               normalZ {bg::get<2> (origin), stdXZ};

    vector<NeuronNode> arborNodes;
    arborNodes.reserve (nodesCount);

    arborNodes.push_back ({Point {bg::get<0> (origin) + normalY (rnd),
                                  bg::get<1> (origin) + f * (layerDist - 3 * stdY + abs (normalY (rnd))),
                                  bg::get<2> (origin) + normalY (rnd)},
                          neuronId, NodeType::axon});

    for (int i = 0; i < nodesCount - 1; ++i)
        arborNodes.push_back ({Point {normalX (rnd), bg::get<1> (origin) + f * (layerDist + normalY (rnd)), normalZ (rnd)},
                              neuronId, NodeType::axon});

    Matrix distances {nodesCount, nodesCount};
    initDistances (distances, arborNodes);

    std::vector<NodeInfo> nodeInfos (nodesCount, {1, 0, numeric_limits<Coord>::max ()});
    nodeInfos[0].isNotProcessed    = 0;
    nodeInfos[0].pathTroughNearest = 0;

    size_t node = 0, nearest;
    vector<pair<size_t, size_t>> arborConnections;
    arborConnections.reserve (nodesCount);
    for (size_t i = 0; i < nodesCount - 1; ++i)
    {
        tie (nearest, node) = findNearest (distances, nodeInfos, node, 0.85);
        assert (node != nearest);
        arborConnections.push_back ({node, nearest});
    }

    // move arbor nodes/connections to the network
    auto const base = nodes.size ();
    copy (arborNodes.cbegin (), arborNodes.cend (), back_inserter (nodes));

    auto rebase = [base] (size_t const id) { return base + id; };
    connections.push_back (make_pair (neuronId, base));
    transform (arborConnections.cbegin (), arborConnections.cend (), back_inserter (connections), [&] (auto const& edge)
    {
        return make_pair (rebase (edge.second), rebase (edge.first));
    });

}

//--------------------------------------------------------------------------------------------------
TestLayer::TestLayer (initializer_list<NeuronNode> const testNodes)
{
    for (auto const& node : testNodes)
        nodes.push_back (node);
}
