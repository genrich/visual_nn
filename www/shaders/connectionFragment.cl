precision mediump float;

uniform vec3 clear_color;
uniform vec3 rest_color;

varying float depth;

void main (void)
{
    gl_FragColor = vec4 (mix (rest_color, clear_color, depth), 0.7);
}
