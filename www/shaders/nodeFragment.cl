void main (void)
{
    if (length ((gl_PointCoord - 0.5) * 2.0) < 0.5)
        gl_FragColor = vec4 (1.0, 0.0, 0.0, 1.0);
    else
        discard;
}
