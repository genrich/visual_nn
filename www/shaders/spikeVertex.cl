attribute vec3 position;
attribute vec3 end_position;
attribute float duration;
attribute float end_time;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;

uniform float log_far_const;
uniform float far;
uniform float time;
uniform float attenuation;

varying float depth;
varying float is_discard;

void main (void)
{
    float spike_time = end_time - attenuation;

    if (spike_time < time)
    {
        is_discard = 1.0;
    }
    else
    {
        is_discard = 0.0;

        gl_Position = pMatrix * mvMatrix * vec4 ((position - end_position) * ((spike_time - time) / duration) + end_position, 1.0);
        gl_PointSize = 4.0;
    }

    float w = clamp (gl_Position.w, 0.0, far);
    depth = log (w * 0.01 + 1.0) / log_far_const;
    depth = depth * depth;
}
