precision mediump float;

uniform vec3 clear_color;
uniform vec3 spike_color;

varying float depth;
varying float is_discard;

void main (void)
{
    if (is_discard == 0.0)
        gl_FragColor = vec4 (mix (spike_color, clear_color, depth), 1.0);
    else 
        discard;
}
