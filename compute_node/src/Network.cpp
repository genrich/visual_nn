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

constexpr coord_type factor = 0.85;

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
pair<int, int>
findNearest (Matrix const&         distances,
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

    int constexpr somaNode = 0;
    std::vector<NodeInfo> nodeInfos (nodesPerNeuron, {1, somaNode, numeric_limits<coord_type>::max ()});
    nodeInfos[somaNode].isNotProcessed    = 0;
    nodeInfos[somaNode].pathTroughNearest = 0;

    int node = somaNode, nearest;
    for (int i = 0; i < nodesPerNeuron - 1; ++i)
    {
        tie (nearest, node) = findNearest (distances, nodeInfos, node, factor);
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
