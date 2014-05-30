attribute vec3 position;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;

void main (void)
{
    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);
    gl_PointSize = 20.0;
}
