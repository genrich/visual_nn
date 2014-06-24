precision mediump float;

varying vec4 color;

void main (void)
{
    if (length ((gl_PointCoord - 0.5) * 2.0) < 1.0)
    {
        gl_FragColor = color;
    }
    else
    {
        discard;
    }
}
