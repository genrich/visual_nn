#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <functional>

#define STRINGIZE(x) STRINGIZE2(x)
#define STRINGIZE2(x) #x
#define assert(x) do {int const val = (x); if (!val) { std::cout << __FILE__ << ":" << STRINGIZE(__LINE__) << ":1: ERROR" << std::endl; exit (EXIT_FAILURE);}} while (0)

#define attribute
#define uniform static
#define varying

// VEC2 --------------------------------------------------------------------------------------------
class vec2
{
    template <int x, int y>
    class array2
    {
        float a[2];
    public:
        operator vec2 ()
        {
            return {a[x], a[y]};
        }
        array2& operator= (vec2 const& v)
        {
            a[x] = v.x;
            a[y] = v.y;
            return *this;
        }
    };

public:
    vec2 (float _x, float _y): x {_x}, y {_y}
    {}
    vec2 (vec2 const& v): x {v.x}, y {v.y}
    {}
    union
    {
        struct { float x, y; };
        float a[2];
        array2<0, 0> xx;
        array2<0, 1> xy;
        array2<1, 0> yx;
        array2<1, 1> yy;
    };

    float& operator[] (int const i)
    {
        return a[i];
    }

    float const& operator[] (int const i) const
    {
        return a[i];
    }

};

// VEC3 --------------------------------------------------------------------------------------------
class vec3
{
    template <int x, int y>
    class array2
    {
        float a[3];
    public:
        operator vec2 ()
        {
            return {a[x], a[y]};
        }
        array2& operator= (vec2 const& v)
        {
            a[x] = v.x;
            a[y] = v.y;
            return *this;
        }
    };

    template <int x, int y, int z>
    class array3
    {
        float a[3];
    public:
        operator vec3 ()
        {
            return {a[x], a[y], a[z]};
        }
        array3& operator= (const vec3& v)
        {
            a[x] = v.x;
            a[y] = v.y;
            a[z] = v.z;
            return *this;
        }
    };
public:
    union
    {
        struct { float x, y, z; };
        struct { float r, g, b; };
        array2<0, 0> xx;
        array2<0, 1> xy;
        array2<0, 2> xz;
        array2<1, 0> yx;
        array2<1, 1> yy;
        array2<1, 2> yz;
        array2<2, 0> zx;
        array2<2, 1> zy;
        array2<2, 2> zz;
        array3<0, 0, 0> xxx;
        array3<0, 0, 1> xxy;
        array3<0, 0, 2> xxz;
        array3<0, 1, 0> xyx;
        array3<0, 1, 1> xyy;
        array3<0, 1, 2> xyz;
        array3<0, 2, 0> xzx;
        array3<0, 2, 1> xzy;
        array3<0, 2, 2> xzz;
        array3<1, 0, 0> yxx;
        array3<1, 0, 1> yxy;
        array3<1, 0, 2> yxz;
        array3<1, 1, 0> yyx;
        array3<1, 1, 1> yyy;
        array3<1, 1, 2> yyz;
        array3<1, 2, 0> yzx;
        array3<1, 2, 1> yzy;
        array3<1, 2, 2> yzz;
        array3<2, 0, 0> zxx;
        array3<2, 0, 1> zxy;
        array3<2, 0, 2> zxz;
        array3<2, 1, 0> zyx;
        array3<2, 1, 1> zyy;
        array3<2, 1, 2> zyz;
        array3<2, 2, 0> zzx;
        array3<2, 2, 1> zzy;
        array3<2, 2, 2> zzz;
    };

    vec3 ()
    {};

    vec3 (float _x, float _y, float _z): x {_x}, y {_y}, z {_z}
    {};

    vec3 operator* (float const f) const
    {
        vec3 v;
        v.x = x * f;
        v.y = y * f;
        v.z = z * f;
        return v;
    };

    vec3 operator+ (vec3 const u) const
    {
        vec3 v;
        v.x = x + u.x;
        v.y = y + u.y;
        v.z = z + u.z;
        return v;
    };
};

class mat4;

// VEC4 --------------------------------------------------------------------------------------------
class vec4
{
public:
    union
    {
        struct { float x, y, z, w; };
        struct { float r, g, b, a; };
        float aa[4];
    };

    vec4 ()
    {};

    vec4 (float _x, float _y, float _z, float _w): x {_x}, y {_y}, z {_z}, w {_w}
    {};

    vec4 (vec3 const& v, float _w): x {v.x}, y {v.y}, z {v.z}, w {_w}
    {};

    vec4& operator= (mat4 const& m)
    {
        return *this;
    }

    float& operator[] (int const i)
    {
        return aa[i];
    }

    float const& operator[] (int const i) const
    {
        return aa[i];
    }

};

// MAT4 --------------------------------------------------------------------------------------------
class mat4
{
public:
    union
    {
        float a00, a10, a20, a30, // column major
              a01, a11, a21, a31,
              a02, a12, a22, a32,
              a03, a13, a23, a33;
        float a[16];
    };

    float& operator[] (int const i)
    {
        return a[i];
    }

    float const& operator[] (int const i) const
    {
        return a[i];
    }

    void perspective (float out, float fovy, float aspect, float near, float far)
    {
        float f  = 1.0 / std::tan (fovy / 2.0),
              nf = 1.0 / (near - far);

        a00 = f / aspect;
        a10 = 0.0;
        a20 = 0.0;
        a30 = 0.0;
        a01 = 0.0;
        a11 = f;
        a21 = 0.0;
        a31 = 0.0;
        a02 = 0.0;
        a12 = 0.0;
        a22 = (far + near) * nf;
        a32 = -1.0;
        a03 = 0.0;
        a13 = 0.0;
        a23 = (2.0 * far * near) * nf;
        a33 = 0.0;
    }

    mat4 operator* (mat4 const& b) const
    {
        mat4 out;
        out.a00 = a00 * b.a00 + a01 * b.a10 + a02 * b.a20 + a03 * b.a30;
        out.a10 = a10 * b.a00 + a11 * b.a10 + a12 * b.a20 + a13 * b.a30;
        out.a20 = a20 * b.a00 + a21 * b.a10 + a22 * b.a20 + a23 * b.a30;
        out.a30 = a30 * b.a00 + a31 * b.a10 + a32 * b.a20 + a33 * b.a30;

        out.a01 = a00 * b.a01 + a01 * b.a11 + a02 * b.a21 + a03 * b.a30;
        out.a11 = a10 * b.a01 + a11 * b.a11 + a12 * b.a21 + a13 * b.a30;
        out.a21 = a20 * b.a01 + a21 * b.a11 + a22 * b.a21 + a23 * b.a30;
        out.a31 = a30 * b.a01 + a31 * b.a11 + a32 * b.a21 + a33 * b.a30;

        out.a02 = a00 * b.a02 + a01 * b.a12 + a02 * b.a22 + a03 * b.a32;
        out.a12 = a10 * b.a02 + a11 * b.a12 + a12 * b.a22 + a13 * b.a32;
        out.a22  =a20 * b.a02 + a21 * b.a12 + a22 * b.a22 + a23 * b.a32;
        out.a32  =a30 * b.a02 + a31 * b.a12 + a32 * b.a22 + a33 * b.a32;

        out.a03 = a00 * b.a03 + a01 * b.a13 + a02 * b.a23 + a03 * b.a33;
        out.a13 = a10 * b.a03 + a11 * b.a13 + a12 * b.a23 + a13 * b.a33;
        out.a23 = a20 * b.a03 + a21 * b.a13 + a22 * b.a23 + a23 * b.a33;
        out.a33 = a30 * b.a03 + a31 * b.a13 + a32 * b.a23 + a33 * b.a33;
        return out;
    }

};

vec4 operator* (mat4 m, vec4 a)
{
    vec4 out;
    out.x = m.a00 * a.x + m.a01 * a.y + m.a02 * a.z + m.a03 * a.w;
    out.y = m.a10 * a.x + m.a11 * a.y + m.a12 * a.z + m.a13 * a.w;
    out.z = m.a20 * a.x + m.a21 * a.y + m.a22 * a.z + m.a23 * a.w;
    out.w = m.a30 * a.x + m.a31 * a.y + m.a32 * a.z + m.a33 * a.w;
    return out;
}

// Functions ---------------------------------------------------------------------------------------
template <typename T>
T mix (T const& x, T const& y, float a)
{
    return {x * (1.0 - a) + y * a};
}

float clamp (float value, float min, float max)
{
    return std::min (std::max (value, min), max);
}

float log (float v)
{
    return std::log (v);
}

// -------------------------------------------------------------------------------------------------
namespace shader
{
vec4 gl_Position;

void main ();
}

using setup    = std::function<void()>;
using teardown = std::function<void()>;
std::vector<std::pair<setup, teardown>> tests ();

int main ()
{
    for (auto const& pair : tests ())
    {
        pair.first ();
        shader::main ();
        pair.second ();
    }
}
