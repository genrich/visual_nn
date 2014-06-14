#include <atomic>

#include "erl_nif.h"

using namespace std;

static atomic<double> speed;

static ERL_NIF_TERM spike_speed (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) return enif_make_badarg (env);

    double value {speed.load ()};

    return enif_make_double (env, value);
}

static ERL_NIF_TERM set_spike_speed (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc == 1)
    {
        double double_value;
        int int_value;

        if (enif_get_double (env, argv[0], &double_value))
        {
            speed.store (double_value);
            return enif_make_double (env, double_value);
        }
        else if (enif_get_int (env, argv[0], &int_value))
        {
            double_value = static_cast<double> (int_value);
            speed.store (double_value);
            return enif_make_double (env, double_value);
        }
    }

    return enif_make_badarg (env);
}

static ErlNifFunc nifs[] =
{
    {"spike_speed",     0, spike_speed},
    {"set_spike_speed", 1, set_spike_speed}
};

ERL_NIF_INIT (vnn_params, nifs, nullptr, nullptr, nullptr, nullptr)
