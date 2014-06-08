attribute vec3 position;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;
uniform float log_far_const;
uniform float far;

varying float depth;

void main (void)
{
    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);

    float w = clamp (gl_Position.w, 0.0, far);
    depth = log (w * 0.001 + 1.0) / log_far_const;
    depth = depth * depth;
}
