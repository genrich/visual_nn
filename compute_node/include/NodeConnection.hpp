#ifndef NODECONNECTION_H
#define NODECONNECTION_H

#include <string>
#include <functional>
#include <thread>
#include <vector>
#include <map>
#include <unordered_map>

#include "erl_interface.h"

#define VNN_CNODE "vnn_cnode"
#define CNODE     "COMPUTE_NODE: "

/**
 * Transport to the erlang node
 */
class Connection
{
public:
    ei_cnode ec;
    int      fd;

    Connection (std::string connectTo, std::string nodeName, std::string erlangCookie);
    ~Connection ();
    Connection (Connection const&)            = delete;
    Connection& operator= (Connection const&) = delete;
};

/**
 * Connection to the erlang node handler.
 */
class NodeConnection
{
    Connection  conn;
    std::thread inboundThread;

    void inbound ();
    void linkToRemote ();
    void createNetwork ();
    void sendAddNode (int const id, int const somaId, std::string const type, float const x, float const y, float const z);
    void sendAddConnection (int const u, int const v);
public:
    NodeConnection (std::string connectTo, std::string nodeName, std::string erlangCookie);
    NodeConnection ();
    ~NodeConnection ();
    NodeConnection (NodeConnection const&)            = delete;
    NodeConnection& operator= (NodeConnection const&) = delete;
};

#endif // NODECONNECTION_H
