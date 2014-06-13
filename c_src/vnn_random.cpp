#include <random>

#include "erl_nif.h"

using namespace std;

static random_device rd;
static mt19937 rnd (rd ());

static ERL_NIF_TERM exponential (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double lambda, interval;
 
    if (argc != 1 || !enif_get_double (env, argv[0], &lambda))
        return enif_make_badarg (env);

    exponential_distribution<> exponential (lambda);
    interval = exponential (rnd);

    return enif_make_double (env, interval);
}

static ERL_NIF_TERM normal (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double mean, std, value;
 
    if (argc != 2 || !enif_get_double (env, argv[0], &mean)
                  || !enif_get_double (env, argv[1], &std))
        return enif_make_badarg (env);

    normal_distribution<> normal (mean, std);
    value = normal (rnd);

    return enif_make_double (env, value);
}

static ERL_NIF_TERM uniform0 (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double value;
 
    if (argc != 0) return enif_make_badarg (env);

    uniform_real_distribution<> uniform (0.0, 1.0);
    value = uniform (rnd);

    return enif_make_double (env, value);
}

static ERL_NIF_TERM uniform2 (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double a, b, value;
 
    if (argc != 2 || !enif_get_double (env, argv[0], &a)
                  || !enif_get_double (env, argv[1], &b))
        return enif_make_badarg (env);

    uniform_real_distribution<> uniform (a, b);
    value = uniform (rnd);

    return enif_make_double (env, value);
}

static ErlNifFunc nifs[] =
{
    {"exponential", 1, exponential},
    {"normal",      2, normal},
    {"uniform",     0, uniform0},
    {"uniform",     2, uniform2}
};

ERL_NIF_INIT (vnn_random, nifs, nullptr, nullptr, nullptr, nullptr)
