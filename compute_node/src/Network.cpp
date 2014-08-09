#include "Network.hpp"

#include <boost/graph/adjacency_matrix.hpp>
#include <boost/graph/prim_minimum_spanning_tree.hpp>
#include <boost/graph/kruskal_min_spanning_tree.hpp>
#include <boost/graph/random_spanning_tree.hpp>

using namespace std;
using namespace boost;

constexpr float stimulusXstart = -200.0f;
constexpr float stimulusXstep  =   20.0f;
constexpr float stimulusZstart =  400.0f;
constexpr float stimulusZstep  =  -20.0f;
constexpr float stimulusY      = -300.0f;

constexpr int nodesPerNeuron = 1000;

constexpr float neuronXmean =  0.0f;
constexpr float neuronXstd  = 50.0f;

using Graph            = adjacency_matrix<undirectedS, no_property, property<edge_weight_t, float>>;
using VertexIterator   = boost::graph_traits<Graph>::vertex_iterator;
using VertexDescriptor = graph_traits<Graph>::vertex_descriptor;
using EdgeDescriptor   = graph_traits<Graph>::edge_descriptor;

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
    if (type == soma || type == stimulus_active || type == stimulus_rest)
        nodeSomaIds.push_back (id);
    else
        nodeSomaIds.push_back (somaId);

    nodeTypes.push_back (type);
    nodes.push_back (x);
    nodes.push_back (y);
    nodes.push_back (z);

    return id++;
}

//--------------------------------------------------------------------------------------------------
void Network::createNeuron ()
{
    int baseId = id;

    normal_distribution<> normal {neuronXmean, neuronXstd};
    lognormal_distribution<> logNormal {0, 0.5};

    int somaId = addNode (0, soma, neuronXmean, stimulusY + 70, neuronXmean);

    for (int i = 0; i < nodesPerNeuron; ++i)
    {
        addNode (somaId, dendrite, normal (rnd), stimulusY + 100 * logNormal (rnd), normal (rnd));
    }

    Graph g {nodesPerNeuron};
    auto vmap = get (vertex_index, g);
    VertexIterator src, end;
    for (tie (src, end) = vertices (g); src != end; ++src)
    {
        for (VertexIterator dest {src + 1}; dest != end; ++dest)
        {
            auto const s = (baseId + vmap[*src])  * 3;
            auto const d = (baseId + vmap[*dest]) * 3;
            float weight = sqrt (pow (static_cast<double> (nodes[s + 0] - nodes[d + 0]), 2.0) + 
                                 pow (static_cast<double> (nodes[s + 1] - nodes[d + 1]), 2.0) +
                                 pow (static_cast<double> (nodes[s + 2] - nodes[d + 2]), 2.0));
            EdgeDescriptor e; bool inserted;
            tie (e, inserted) = add_edge (*src, *dest, weight, g);
        }
    }

    vector<VertexDescriptor> p (num_vertices (g));

    prim_minimum_spanning_tree (g, &p[0]);
    // random_spanning_tree (g, rnd, predecessor_map (&p[0]));
    for (int i = 1; i < p.size (); ++i)
        connections.push_back ({baseId + vertex (i, g), baseId + p[i]});

    // vector<EdgeDescriptor> tree;
    // kruskal_minimum_spanning_tree (g, back_inserter (tree));
    // for (auto const e : tree)
    //     connections.push_back ({baseId + source (e, g), baseId + target (e, g)});
}

//--------------------------------------------------------------------------------------------------
Network::Network ()
{
}

//--------------------------------------------------------------------------------------------------
void Network::init ()
{
    id = 0;
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
