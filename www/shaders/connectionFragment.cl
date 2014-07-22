precision mediump float;

uniform vec3 clear_color;
uniform vec3 connection_color;

varying float depth;

void main (void)
{
    gl_FragColor = vec4 (mix (connection_color, clear_color, depth), 0.7);
}
