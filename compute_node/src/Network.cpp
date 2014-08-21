#include "Network.hpp"

using namespace std;
using namespace boost::numeric::ublas;

constexpr coord_type stimulusXstart = -200.0;
constexpr coord_type stimulusXstep  =   20.0;
constexpr coord_type stimulusZstart =  400.0;
constexpr coord_type stimulusZstep  =  -20.0;
constexpr coord_type stimulusY      = -300.0;

constexpr int nodesPerNeuron = 1000;

constexpr coord_type neuronXmean =  0.0;
constexpr coord_type neuronXstd  = 50.0;

constexpr float factor = 0.9f;

std::vector<std::vector<int>> const stimulusArray
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
tuple<int, int>
initNearestPair (Matrix const& distances, std::vector<NodeInfo>& nodeInfos)
{
    coord_type minDistance = numeric_limits<coord_type>::max ();
    int minIndex = 0;
    int constexpr centerNode = 0;

    nodeInfos[centerNode].isNotProcessed = 0;

    for (int i = centerNode + 1; i < nodeInfos.size (); ++i)
    {
        nodeInfos[i].isNotProcessed      = 1;
        nodeInfos[i].nearestNode         = centerNode;
        nodeInfos[i].pathTroughNearest   = distances (centerNode, i);

        if (nodeInfos[i].pathTroughNearest <= minDistance)
        {
            minDistance = nodeInfos[i].pathTroughNearest;
            minIndex    = i;
        }
    }

    nodeInfos[minIndex].isNotProcessed = 0;
    return make_tuple (nodeInfos[minIndex].nearestNode, minIndex);
}

//--------------------------------------------------------------------------------------------------
tuple<int, int>
findNearestPair (Matrix const&         distances,
                std::vector<NodeInfo>& nodeInfos,
                int const              node,
                coord_type const       factor)
{
    int        minIndex       = 0;
    coord_type minDistance    = numeric_limits<coord_type>::max ();
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
            }
            if (d < minDistance)
            {
                minDistance = d;
                minIndex    = i;
            }
        }
    }

    nodeInfos[minIndex].isNotProcessed = 0;
    return make_tuple (nodeInfos[minIndex].nearestNode, minIndex);
}

//--------------------------------------------------------------------------------------------------
void Network::createNeuron ()
{
    normal_distribution<> normal {neuronXmean, neuronXstd};
    lognormal_distribution<> logNormal {0.0, 0.5};

    int somaId = nodes.size ();

    std::vector<NeuronNode> neuronNodes;
    neuronNodes.push_back ({somaId, soma, Point {neuronXmean, stimulusY + 70.0, neuronXmean}});

    for (int i = 1; i < nodesPerNeuron; ++i)
        neuronNodes.push_back ({somaId, dendrite, Point {static_cast<coord_type> (normal (rnd)),
                                                         static_cast<coord_type> (stimulusY + 100.0 * logNormal (rnd)),
                                                         static_cast<coord_type> (normal (rnd))}});

    Matrix distances {nodesPerNeuron, nodesPerNeuron};
    initDistances (distances, neuronNodes);

    std::vector<NodeInfo> nodeInfos (nodesPerNeuron);

    int center, nearest, node;
    tie (center, nearest) = initNearestPair (distances, nodeInfos);

    connections.push_back ({somaId + center, somaId + nearest});

    for (int i = 2; i < nodesPerNeuron; ++i)
    {
        tie (node, nearest) = findNearestPair (distances, nodeInfos, nearest, factor);
        connections.push_back ({somaId + node, somaId + nearest});
    }

    nodes.insert (nodes.end (), neuronNodes.begin (), neuronNodes.end ());
}

//--------------------------------------------------------------------------------------------------
Network::Network ()
{
}

//--------------------------------------------------------------------------------------------------
void Network::init ()
{
    nodes.clear ();
    connections.clear ();
}

//--------------------------------------------------------------------------------------------------
void Network::createStimulus ()
{
    for (int i = 0; i < stimulusArray.size (); ++i)
        for (int j = 0; j < stimulusArray[i].size (); ++j)
        {
            int const somaId = nodes.size ();
            if (stimulusArray[i][j])
                nodes.push_back ({somaId, stimulus_active,
                                  Point {stimulusXstart + i * stimulusXstep, stimulusY, stimulusZstart + j * stimulusZstep}});
            else
                nodes.push_back ({somaId, stimulus_rest,
                                  Point {stimulusXstart + i * stimulusXstep, stimulusY, stimulusZstart + j * stimulusZstep}});
        }
}

//--------------------------------------------------------------------------------------------------
void Network::createNetwork ()
{
    createNeuron ();
}
