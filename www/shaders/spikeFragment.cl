precision mediump float;

uniform vec3 spike_color;

varying float is_discard;

void main (void)
{
    if (is_discard == 0.0)
        gl_FragColor = vec4 (spike_color, 1.0);
    else 
        discard;
}
