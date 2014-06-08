precision mediump float;

uniform vec3 clear_color;
uniform vec3 rest_color;
uniform vec3 spike_color;

varying float depth;
varying float spike_strength;

void main (void)
{
    if (length ((gl_PointCoord - 0.5) * 2.0) < 1.0)
    {
        vec3 color = mix (rest_color, spike_color, spike_strength);
        gl_FragColor = vec4 (mix (color, clear_color, depth), 1.0);
    }
    else
    {
        discard;
    }
}
