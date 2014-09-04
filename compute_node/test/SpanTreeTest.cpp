#include <string>

#include "Network.hpp"

#define BOOST_TEST_MODULE SpanTreeTest

#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>

using namespace std;

BOOST_AUTO_TEST_CASE (FindNearestTest)
{
    Coord constexpr factor = 0.8;
    int constexpr somaNode = 0;
    std::vector<NeuronNode> nodes;
    nodes.push_back ({somaNode, soma,     Point {0, 0, 0}});
    nodes.push_back ({somaNode, dendrite, Point {1, 0, 0}});
    nodes.push_back ({somaNode, dendrite, Point {3, 0, 0}});
    nodes.push_back ({somaNode, dendrite, Point {6, 0, 0}});
    nodes.push_back ({somaNode, dendrite, Point {2, 2, 0}});

    Matrix distances {nodes.size (), nodes.size ()};
    initDistances (distances, nodes);

    std::vector<NodeInfo> nodeInfos (nodes.size (), {1, somaNode, numeric_limits<Coord>::max ()});
    nodeInfos[somaNode].isNotProcessed    = 0;
    nodeInfos[somaNode].pathTroughNearest = 0;

    int nearest, node;
    tie (nearest, node) = findNearest (distances, nodeInfos, somaNode, factor);
    BOOST_CHECK_EQUAL (nearest, 0);
    BOOST_CHECK_EQUAL (node,    1);

    tie (nearest, node) = findNearest (distances, nodeInfos, node, factor);
    BOOST_CHECK_EQUAL (nearest, 1);
    BOOST_CHECK_EQUAL (node,    2);

    tie (nearest, node) = findNearest (distances, nodeInfos, node, factor);
    BOOST_CHECK_EQUAL (nearest, 1);
    BOOST_CHECK_EQUAL (node,    4);

    tie (nearest, node) = findNearest (distances, nodeInfos, node, factor);
    BOOST_CHECK_EQUAL (nearest, 2);
    BOOST_CHECK_EQUAL (node,    3);
}
