attribute vec3 position;
attribute float id;
attribute float attributes;

uniform mat4 mvMatrix;
uniform mat4 pMatrix;
uniform float log_far_const;
uniform float far;
uniform float point_size;

varying vec4 color;

void main (void)
{
    gl_Position = pMatrix * mvMatrix * vec4 (position, 1.0);

    color = vec4 (mod (id,                   256.0) / 255.0,
                  mod (floor (id / 256.0),   256.0) / 255.0,
                  mod (floor (id / 65536.0), 256.0) / 255.0,
                  1.0);

    float nodeType = mod (floor (attributes / 33554432.0), 4.0);

    float w = clamp (gl_Position.w, 0.0, far);
    float depth = log (w * 0.01 + 1.0) / log_far_const;

    if (nodeType == 0.0) // Soma
    {
        gl_PointSize = point_size * (1.0 - depth);
    }
    else if (nodeType == 1.0) // Synapse
    {
        gl_PointSize = 0.25 * point_size * (1.0 - depth);
    }
    else // Dendrite, Axon
    {
        gl_PointSize = 1.0;
    }
}
