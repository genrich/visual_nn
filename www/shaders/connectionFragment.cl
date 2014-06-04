precision mediump float;

uniform vec3 rest_color;

void main (void)
{
    gl_FragColor = vec4 (rest_color, 1.0);
}
