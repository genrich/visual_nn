precision mediump float;

uniform vec3 rest_color;
uniform vec3 spike_color;

varying float spike_strength;

void main (void)
{
    if (length ((gl_PointCoord - 0.5) * 2.0) < 0.5)
    {
        vec3 color = spike_strength * spike_color + (1.0 - spike_strength) * rest_color;
        gl_FragColor = vec4 (color, 1.0);
    }
    else
    {
        discard;
    }
}
