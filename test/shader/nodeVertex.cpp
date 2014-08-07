#include "ut_shader.hpp"
namespace shader
{
#include "../../www/shaders/nodeVertex.cl"
}

using namespace std;

vector<pair<setup, teardown>> tests ()
{
    shader::log_far_const = 1.0;
    shader::point_size = 10.0;

    return {
    { // Soma
        [] {
            shader::attributes = 33554432.0 * 0;
        },
        [] {
            assert (shader::gl_PointSize == 10.0);
        }
    },

    { // Synapse
        [] {
            shader::attributes = 33554432.0 * 1;
        },
        [] {
            assert (shader::gl_PointSize == 2.5);
        }
    },

    { // Dendrite
        [] {
            shader::attributes = 33554432.0 * 2;
        },
        [] {
            assert (shader::gl_PointSize == 1.0);
        }
    },

    { // Axon
        [] {
            shader::attributes = 33554432.0 * 3;
        },
        [] {
            assert (shader::gl_PointSize == 1.0);
        }
    }};
}
