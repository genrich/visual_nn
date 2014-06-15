attribute vec3 position;
attribute float end_time;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;
uniform float time;
uniform float attenuation;
uniform float log_far_const;
uniform float far;
uniform float point_size;

varying float spike_strength;
varying float depth;

void main (void)
{
    if (time > end_time)
    {
        spike_strength = 0.0;
    }
    else
    {
        float t = (end_time - time) / attenuation;
        float t1 = t * t;
        float t2 = t * t;
        spike_strength = t1 * t2;
    }
    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);

    float w = clamp (gl_Position.w, 0.0, far);
    depth = log (w * 0.01 + 1.0) / log_far_const;

    gl_PointSize = point_size * (1.0 - depth);

    depth = depth * depth;
}
