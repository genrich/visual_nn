#include <iostream>
#include <stdexcept>
#include <cassert>
#include <future>

#include "NodeConnection.hpp"

using namespace std;

int main (int argc, char* argv[])
{
    assert (sizeof (float) == 4);
    assert (* (uint16_t *)"\x00\xff" == 0xff00); // little endian

    try
    {
        this_thread::sleep_for (chrono::seconds {1});

        NodeConnection node;
    }
    catch (exception const& e) { cout << CNODE << "Error: " << e.what () << endl; exit (EXIT_FAILURE); }
    catch (...)                { exit (EXIT_FAILURE); }
    return EXIT_SUCCESS;
}
