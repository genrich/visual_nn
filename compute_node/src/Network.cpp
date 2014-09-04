#include <random>

#include "Network.hpp"

using namespace std;

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
void transform (vector<NeuronNode> const&     nodesFrom,
                vector<NeuronNode>&           nodesTo,
                vector<pair<int, int>> const& connectionsFrom,
                vector<pair<int, int>>&       connectionsTo)

{
    int const base = static_cast<int> (nodesTo.size ());

    transform (nodesFrom.cbegin (), nodesFrom.cend (), back_inserter (nodesTo), [=] (auto const& node)
    {
        return NeuronNode {base + node.somaId, node.type, node.point};
    });

    transform (connectionsFrom.cbegin (), connectionsFrom.cend (), back_inserter (connectionsTo), [=] (auto const& edge)
    {
        return make_pair (base + edge.first, base + edge.second);
    });
}

//--------------------------------------------------------------------------------------------------
void
initDistances (Matrix& distances, std::vector<NeuronNode> const& nodes)
{
    for (int row = 0; row < distances.size1 (); ++row)
    {
        for (int col = 0; col < row; ++col)
        {
            auto const d = bg::distance (nodes[row].point, nodes[col].point);
            distances (row, col) = d;
            distances (col, row) = d;
        }
        distances (row, row) = 0;
    }
}

//--------------------------------------------------------------------------------------------------
pair<int, int>
findNearest (Matrix const&         distances,
            std::vector<NodeInfo>& nodeInfos,
            int const              node,
            Coord const            factor)
{
    int        minIndex       = 0;
    Coord minDistance         = numeric_limits<Coord>::max ();
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
            int const somaId = nodes.size ();
            if (activityMap[i][j])
                nodes.push_back ({somaId, stimulus_active, Point {minX + i * stepX, y, minZ + j * stepZ}});
            else
                nodes.push_back ({somaId, stimulus_rest,   Point {minX + i * stepX, y, minZ + j * stepZ}});
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
        createNeuron (Point {uniformX (rnd), uniformY (rnd), uniformZ (rnd)});
}

//--------------------------------------------------------------------------------------------------
void UniformLayer::createNeuron (Point const& center)
{
    int constexpr nodesPerNeuron = 300;
    Coord constexpr std = 50.0;
    Coord constexpr factor = 0.85;

    Coord const centerX = bg::get<0> (center),
                centerY = bg::get<1> (center),
                centerZ = bg::get<2> (center);

    normal_distribution<> normalX {centerX, std},
                          normalZ {centerZ, std};
    lognormal_distribution<> logNormal {0.0, 0.5};

    int constexpr somaId = 0;
    vector<NeuronNode>     neuronNodes {{somaId, soma, Point {centerX, centerY, centerZ}}}; // with initial soma node
    vector<pair<int, int>> neuronConnections;

    for (int i = 0; i < nodesPerNeuron - 1; ++i)
        neuronNodes.push_back ({somaId, dendrite, Point {static_cast<Coord> (normalX (rnd)),
                                                         static_cast<Coord> (centerY - 100.0 + 100.0 * logNormal (rnd)),
                                                         static_cast<Coord> (normalZ (rnd))}});

    Matrix distances {nodesPerNeuron, nodesPerNeuron};
    initDistances (distances, neuronNodes);

    std::vector<NodeInfo> nodeInfos (nodesPerNeuron, {1, somaId, numeric_limits<Coord>::max ()});
    nodeInfos[somaId].isNotProcessed    = 0;
    nodeInfos[somaId].pathTroughNearest = 0;

    int node = somaId, nearest;
    for (int i = 0; i < nodesPerNeuron - 1; ++i)
    {
        tie (nearest, node) = findNearest (distances, nodeInfos, node, factor);
        neuronConnections.push_back ({somaId + node, somaId + nearest});
    }

    transform (neuronNodes, nodes, neuronConnections, connections);
}
