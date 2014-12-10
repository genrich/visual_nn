#include <string>

#include "Network.hpp"

#define BOOST_TEST_MODULE NetworkTest

#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>

using namespace std;

BOOST_AUTO_TEST_CASE (NetworkTest)
{
    Network network {StimulusLayer {BoundingBox {Point {0, 0, 0}, Point {0, 0, 0}}, ActivityMap {{}}},
                     UniformLayer {BoundingBox {Point {0, 0, 0}, Point {10, 10, 10}}, NeuronCount {10}, 1},
                     UniformLayer {BoundingBox {Point {0, 0, 0}, Point {10, 10, 10}}, NeuronCount {10}, 1},
                     UniformLayer {BoundingBox {Point {0, 0, 0}, Point {10, 10, 10}}, NeuronCount {10}, 1}};

    BOOST_CHECK (true);
}
