#include <string>

#include "Network.hpp"

#define BOOST_TEST_MODULE SpanTreeTest

#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>

using namespace std;
using namespace boost::numeric::ublas;

BOOST_AUTO_TEST_CASE (FindNearestPairTest)
{
    std::vector<NeuronNode> nodes;
    nodes.push_back ({0, soma,     Point {0, 0, 0}});
    nodes.push_back ({0, dendrite, Point {1, 0, 0}});
    nodes.push_back ({0, dendrite, Point {3, 0, 0}});
    nodes.push_back ({0, dendrite, Point {6, 0, 0}});

    Matrix distances {nodes.size (), nodes.size ()};
    initDistances (distances, nodes);

    std::vector<NodeInfo> nodeInfos (nodes.size ());
    int node1, node2; coord_type len;
    tie (node1, node2) = initNearestPair (distances, nodeInfos);
    BOOST_CHECK_EQUAL (node1, 0);
    BOOST_CHECK_EQUAL (node2, 1);

    tie (node1, node2) = findNearestPair (distances, nodeInfos, node2, 0.9);
    BOOST_CHECK_EQUAL (node1, 1);
    BOOST_CHECK_EQUAL (node2, 2);

    tie (node1, node2) = findNearestPair (distances, nodeInfos, node2, 0.9);
    BOOST_CHECK_EQUAL (node1, 2);
    BOOST_CHECK_EQUAL (node2, 3);
}

BOOST_AUTO_TEST_CASE (NetworkTest)
{
    Network network;

    network.init ();
    network.createStimulus ();
    network.createNetwork ();

    BOOST_CHECK (true);
}
