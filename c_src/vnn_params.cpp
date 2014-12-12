#include <atomic>
#include <chrono>

#include "erl_nif.h"

using namespace std;
using namespace chrono;

#define PARAMS_MUL \
    X (absolute_refractory)

#define PARAMS_DIV  \
    X (spike_speed) \
    X (active_rate) \
    X (rest_rate)

#define PARAMS   \
    X (slowdown) \
    PARAMS_DIV   \
    PARAMS_MUL

#define X(param) static atomic<double> param {1.0};
PARAMS
#undef X

#define X(param)                                                                          \
    static ERL_NIF_TERM get_##param (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
    {                                                                                     \
        return enif_make_double (env, param);                                             \
    }
PARAMS
#undef X

#define X(param)                                                                          \
    static ERL_NIF_TERM set_##param (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
    {                                                                                     \
        double value;                                                                     \
        if (argc == 1 && enif_get_double (env, argv[0], &value))                          \
        {                                                                                 \
            param = value * slowdown;                                                     \
            return enif_make_atom (env, "ok");                                            \
        }                                                                                 \
        return enif_make_badarg (env);                                                    \
    }
PARAMS_MUL
#undef X

#define X(param)                                                                          \
    static ERL_NIF_TERM set_##param (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
    {                                                                                     \
        double value;                                                                     \
        if (argc == 1 && enif_get_double (env, argv[0], &value))                          \
        {                                                                                 \
            param = value / slowdown;                                                     \
            return enif_make_atom (env, "ok");                                            \
        }                                                                                 \
        return enif_make_badarg (env);                                                    \
    }
PARAMS_DIV
#undef X

static ERL_NIF_TERM set_slowdown (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double value;
    if (argc == 1 && enif_get_double (env, argv[0], &value))
    {
        double old_value = slowdown.exchange (value);
#define X(param) param = param / old_value * value;
        PARAMS_MUL
#undef X
#define X(param) param = param * old_value / value;
        PARAMS_DIV
#undef X
        return enif_make_atom (env, "ok");
    }
    return enif_make_badarg (env);
}

static ERL_NIF_TERM now (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_double (env, duration_cast<duration<double>> (high_resolution_clock::now ().time_since_epoch ()).count ());
}

static ErlNifFunc nifs[] =
{
#define X(param) {#param, 0, get_##param}, {"set_"#param, 1, set_##param},
    PARAMS
#undef X
    {"now", 0, now}
};

ERL_NIF_INIT (vnn_params, nifs, nullptr, nullptr, nullptr, nullptr)
