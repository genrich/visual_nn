#ifndef NODECONNECTION_H
#define NODECONNECTION_H

#include <string>
#include <functional>
#include <thread>
#include <vector>
#include <map>
#include <unordered_map>

#include "erl_interface.h"

#include "Network.hpp"

#define VNN_CNODE "vnn_cnode"
#define CNODE     "COMPUTE_NODE: "

//--------------------------------------------------------------------------------------------------
class BufferGuard
{
public:
    ei_x_buff& x;

    explicit BufferGuard (ei_x_buff &x_) :x (x_) {}
    ~BufferGuard () { ei_x_free (&x); }
    BufferGuard (BufferGuard const&)            = delete;
    BufferGuard& operator= (BufferGuard const&) = delete;
};

//--------------------------------------------------------------------------------------------------
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

//--------------------------------------------------------------------------------------------------
// Represents connection to the erlang node.
//--------------------------------------------------------------------------------------------------
class NodeConnection
{
    Connection  conn;
    std::thread inboundThread;
    Network     network;

    void inbound ();
    void linkToRemote ();
    void createNetwork ();
    void sendAddNode (int const id, int const somaId, std::string const type, float const x, float const y, float const z);
public:
    NodeConnection (std::string connectTo, std::string nodeName, std::string erlangCookie);
    NodeConnection ();
    ~NodeConnection ();
    NodeConnection (NodeConnection const&)            = delete;
    NodeConnection& operator= (NodeConnection const&) = delete;
};

#endif // NODECONNECTION_H
