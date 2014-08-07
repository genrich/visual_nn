#include "Network.hpp"

using namespace std;

constexpr float stimulusXstart = -200.0f;
constexpr float stimulusXstep  =   20.0f;
constexpr float stimulusZstart =  400.0f;
constexpr float stimulusZstep  =  -20.0f;
constexpr float stimulusY      = -300.0f;

constexpr int nodesPerNeuron = 10;

constexpr float neuronXmean =  0.0f;
constexpr float neuronXstd  = 50.0f;

vector<vector<int>> const stimulusArray
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
int Network::addNode (int const somaId, NodeType const type, float const x, float const y, float const z)
{
    int id = nodeTypes.size ();

    if (type == soma || type == stimulus_active || type == stimulus_rest)
        nodeSomaIds.push_back (id);
    else
        nodeSomaIds.push_back (somaId);

    nodeTypes.push_back (type);
    nodes.push_back (x);
    nodes.push_back (y);
    nodes.push_back (z);

    return id;
}

//--------------------------------------------------------------------------------------------------
void Network::createNeuron ()
{
    normal_distribution<> normal {neuronXmean, neuronXstd};
    lognormal_distribution<> logNormal {0, 0.5};

    int somaId = addNode (0, soma, neuronXmean, stimulusY + 70, neuronXmean);

    for (int i = 0; i < nodesPerNeuron; ++i)
    {
        addNode (somaId, dendrite, normal (rnd), stimulusY + 100 * logNormal (rnd), normal (rnd));
    }
}

//--------------------------------------------------------------------------------------------------
Network::Network ()
{
}

//--------------------------------------------------------------------------------------------------
void Network::init ()
{
    nodeSomaIds.clear ();
    nodeTypes.clear ();
    nodes.clear ();
}

//--------------------------------------------------------------------------------------------------
void Network::createStimulus ()
{
    for (int i = 0; i < stimulusArray.size (); ++i)
        for (int j = 0; j < stimulusArray[i].size (); ++j)
            if (stimulusArray[i][j])
                addNode (0, stimulus_active, stimulusXstart + i * stimulusXstep, stimulusY, stimulusZstart + j * stimulusZstep);
            else
                addNode (0, stimulus_rest,   stimulusXstart + i * stimulusXstep, stimulusY, stimulusZstart + j * stimulusZstep);
}

//--------------------------------------------------------------------------------------------------
void Network::createNetwork ()
{
    createNeuron ();
}
