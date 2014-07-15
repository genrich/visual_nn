precision mediump float;

uniform vec3 clear_color;

varying vec3 color;
varying float depth;
varying float is_discard;

void main (void)
{
    if (is_discard == 0.0)
        gl_FragColor = vec4 (mix (color, clear_color, depth), 0.7);
    else 
        discard;
}
