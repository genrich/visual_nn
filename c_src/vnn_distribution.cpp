#include <random>

#include "erl_nif.h"

using namespace std;

static default_random_engine generator;

static ERL_NIF_TERM exponential (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double lambda, interval;
 
    if (argc != 1 || !enif_get_double (env, argv[0], &lambda))
        return enif_make_badarg (env);

    exponential_distribution<> exponential (lambda);
    interval = exponential (generator);

    return enif_make_double (env, interval);
}

static ErlNifFunc nifs[] =
{
    {"exponential", 1, exponential}
};

ERL_NIF_INIT (vnn_distribution, nifs, nullptr, nullptr, nullptr, nullptr)
