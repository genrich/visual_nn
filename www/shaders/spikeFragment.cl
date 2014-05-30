precision mediump float;

varying float is_discard;

void main (void)
{
    if (is_discard == 0.0)
        gl_FragColor = vec4 (0.0, 1.0, 0.0, 1.0);
    else 
        discard;
}
