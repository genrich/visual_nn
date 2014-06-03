attribute vec3 position;
attribute vec3 end_position;
attribute float duration;
attribute float end_time;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;
uniform float time;

varying float is_discard;

void main (void)
{
    if (time > end_time)
    {
        is_discard = 1.0;
    }
    else
    {
        is_discard = 0.0;

        gl_Position = pMatrix * mvMatrix * vec4 ((position - end_position) * ((end_time - time) / duration) + end_position, 1.0);
        gl_PointSize = 4.0;
    }
}
