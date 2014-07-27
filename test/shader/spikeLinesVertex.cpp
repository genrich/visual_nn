#include "ut_shader.hpp"
namespace shader
{
#include "../../www/shaders/spikeLinesVertex.cl"
}

using namespace std;

vector<pair<setup, teardown>> tests ()
{
    shader::spike_color = vec3 {1.0, 0.0, 0.0};
    shader::connection_color  = vec3 {0.0, 1.0, 0.0};
    shader::attenuation = 1.0;

    return {
    { // spike didn't move yet at start endpoint
        [] {
            shader::time     = 0.0;
            shader::end_time = 1.0;
            shader::duration = 0.0;
        },
        [] {
            assert (shader::color.r == 1.0); // at max spike
        }
    },
    { // spike moved for 1 sec
        [] {
            shader::time     = 1.0;
            shader::end_time = 1.0;
            shader::duration = 0.0;
        },
        [] {
            assert (shader::color.g == 1.0); // after attenuation is at rest
        }
    },
    { // spike arrived at mid endpoint
        [] {
            shader::time     = 2.0;
            shader::end_time = 5.0;
            shader::duration = 4.0;
        },
        [] {
            assert (shader::color.r == 0.5); // at mid spike
        }
    },
    { // spike arrived at end endpoint
        [] {
            shader::time     = 4.0;
            shader::end_time = 5.0;
            shader::duration = 4.0;
        },
        [] {
            assert (shader::color.r == 1.0); // at max spike
        }
    },
    { // spike passed end endpoint
        [] {
            shader::time     = 5.0;
            shader::end_time = 5.0;
            shader::duration = 4.0;
        },
        [] {
            assert (shader::color.g == 1.0); // after attenuation at rest
        }
    }};
}
