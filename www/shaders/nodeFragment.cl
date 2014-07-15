precision mediump float;

uniform vec3 clear_color;

varying vec3 color;
varying float depth;

void main (void)
{
    if (length ((gl_PointCoord - 0.5) * 2.0) < 1.0)
    {
        gl_FragColor = vec4 (mix (color, clear_color, depth), 1.0);
    }
    else
    {
        discard;
    }
}
