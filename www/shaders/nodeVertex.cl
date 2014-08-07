attribute vec3 position;
attribute float end_time;
attribute float attributes;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;

uniform float time;
uniform float attenuation;
uniform float log_far_const;
uniform float far;
uniform float point_size;

uniform vec3 rest_color;
uniform vec3 spike_color;

varying vec3 color;
varying float depth;

void main (void)
{
    vec3 attr_color = vec3 (mod (attributes,                   256.0) / 255.0,
                            mod (floor (attributes / 256.0),   256.0) / 255.0,
                            mod (floor (attributes / 65536.0), 256.0) / 255.0);

    float isCustomColor = mod (floor (attributes / 16777216.0), 2.0);

    float nodeType = mod (floor (attributes / 33554432.0), 4.0);

    float end_time_with_attenuation = end_time + attenuation;

    if (end_time_with_attenuation < time)
    {
        color = isCustomColor == 0.0 ? rest_color : attr_color;
    }
    else
    {
        float t = (end_time_with_attenuation - time) / attenuation;
        float t1 = t * t;
        float t2 = t * t;
        float spike_strength = t1 * t2;

        color = isCustomColor == 0.0 ? mix (rest_color, spike_color, spike_strength) : attr_color;
    }

    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);

    float w = clamp (gl_Position.w, 0.0, far);
    depth = log (w * 0.01 + 1.0) / log_far_const;

    if (nodeType == 0.0) // Soma
    {
        gl_PointSize = point_size * (1.0 - depth);
    }
    else if (nodeType == 1.0) // Synapse
    {
        gl_PointSize = 0.25 * point_size * (1.0 - depth);
    }
    else // Dendrite, Axon
    {
        gl_PointSize = 1.0;
    }

    depth = depth * depth;
}
