#include <string>

#include "nanoflann.hpp"
#include "Network.hpp"

#define BOOST_TEST_NO_MAIN KDTeeTest

#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>

using namespace std;
using namespace nanoflann;

BOOST_AUTO_TEST_CASE (RadiusSearchTest)
{
    int constexpr somaNode = 0;
    Network network {TestLayer {{Point {0, 0, 0}, somaNode, NodeType::soma},
                                {Point {1, 0, 0}, somaNode, NodeType::dendrite},
                                {Point {2, 0, 0}, somaNode, NodeType::dendrite},
                                {Point {3, 0, 0}, somaNode, NodeType::dendrite},
                                {Point {4, 0, 0}, somaNode, NodeType::dendrite}}};

	using KDTree = KDTreeSingleIndexAdaptor<L2_Simple_Adaptor<Coord, Network>, Network, 3>;

	KDTree index {3, network, KDTreeSingleIndexAdaptorParams {10}};
	index.buildIndex ();

    Coord constexpr radius = 1.1;
    Point origin {2, 0, 0};

    vector<pair<size_t, Coord>> result;

    auto const nMatches = index.radiusSearch (reinterpret_cast<Coord*> (&origin), radius, result, SearchParams {});

    BOOST_CHECK_EQUAL (nMatches, 3);

    BOOST_CHECK_EQUAL (result[0].first, 2);
    BOOST_CHECK_EQUAL (result[1].first, 1);
    BOOST_CHECK_EQUAL (result[2].first, 3);
}
