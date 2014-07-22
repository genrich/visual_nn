attribute vec3 position;
attribute float duration;
attribute float end_time;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;

uniform float log_far_const;
uniform float far;
uniform float time;
uniform float attenuation;

uniform vec3 connection_color;
uniform vec3 spike_color;

varying vec3 color;
varying float depth;
varying float is_discard;

void main (void)
{
    float spike_time = end_time - attenuation;

    is_discard = 0.0;
    if (end_time < time || time < spike_time - duration)
    {
        is_discard = 1.0;
    }
    if (0.0 < duration && time < spike_time)
    {
        color = mix (spike_color, connection_color, (spike_time - time) / duration);
    }
    else
    {
        color = mix (connection_color, spike_color, (end_time - time) / attenuation);
    }

    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);

    float w = clamp (gl_Position.w, 0.0, far);
    depth = log (w * 0.01 + 1.0) / log_far_const;
    depth = depth * depth;
}
