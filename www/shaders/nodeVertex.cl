attribute vec3 position;
attribute float end_time;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;
uniform float time;
uniform float duration;

varying float spike_strength;

void main (void)
{
    if (time > end_time)
    {
        spike_strength = 0.0;
    }
    else
    {
        float t = (end_time - time) / duration;
        float t1 = t * t;
        float t2 = t * t;
        spike_strength = t1 * t2;
    }
    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);
    gl_PointSize = 20.0;
}
