#ifndef NODECONNECTION_H
#define NODECONNECTION_H

#include <string>
#include <functional>
#include <thread>
#include <vector>
#include <map>

#include "erl_interface.h"

#define VNN_UTILS "vnn_utils"
#define CNODE     "COMPUTE_NODE: "

class BufferGuard
{
public:
    ei_x_buff& x;

    explicit BufferGuard (ei_x_buff &x_) :x (x_) {}
    ~BufferGuard () { ei_x_free (&x); }
    BufferGuard (BufferGuard const&) = delete;
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
};

//--------------------------------------------------------------------------------------------------
// Represents connection to the erlang node.
//--------------------------------------------------------------------------------------------------
class NodeConnection
{
    Connection conn;

    std::thread inboundThread;
    std::thread outboundThread;
    
    void inbound ();
    void outbound ();
    void linkToRemote ();
public:
    NodeConnection (std::string connectTo, std::string nodeName, std::string erlangCookie);
    NodeConnection ();
    ~NodeConnection ();
    NodeConnection (NodeConnection const&)            = delete;
    NodeConnection& operator= (NodeConnection const&) = delete;
    void shutdown ();
};

#endif // NODECONNECTION_H
