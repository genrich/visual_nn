#include <fstream>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

#include "NodeConnection.hpp"

using namespace std;

namespace node { bool shutdown = false; }

string error_msg ()
{
    return string (strerror (erl_errno)) + " (" + to_string (erl_errno) + ")";
}

string default_erlang_cookie ()
{
    string userHome (getenv ("HOME"));
    ifstream file {userHome + "/.erlang.cookie"};
    string cookie {istreambuf_iterator<char> {file}, istreambuf_iterator<char> {}};
    return cookie;
}

string localhost ()
{
    char hostname[1024];
    hostname[1023] = '\0';
    if (gethostname (hostname, 1023) == 0)
        return {hostname};
    else
        return {};
}

//--------------------------------------------------------------------------------------------------
Connection::Connection (string connectTo, string nodeName, string erlangCookie)
{
    erl_init (nullptr, 0);
    
    if (ei_connect_init (&ec, nodeName.c_str (), erlangCookie.c_str (), 0) < 0)
        throw runtime_error ("initializing: " + error_msg ());

    fd = ei_connect (&ec, (char*) connectTo.c_str ());

    if (fd < 0)
        throw runtime_error ("connecting: " + error_msg ());
}

Connection::~Connection ()
{
    shutdown (fd, SHUT_RDWR);
    close (fd);
}

//--------------------------------------------------------------------------------------------------
NodeConnection::NodeConnection (string connectTo, string nodeName, string erlangCookie):
    conn {connectTo, nodeName, erlangCookie},
    inboundThread  {&NodeConnection::inbound,  this},
    outboundThread {&NodeConnection::outbound, this}
{
}

NodeConnection::NodeConnection (): NodeConnection ("visual_nn@" + localhost (), "compute_node", default_erlang_cookie ())
{
}

NodeConnection::~NodeConnection ()
{
    inboundThread.join ();
    outboundThread.join ();
}

void NodeConnection::shutdown ()
{
    node::shutdown = true;
}

void NodeConnection::inbound ()
{
    int version, arity, type, byteLength, index = 0;
    char atom[MAXATOMLEN];
    ei_x_buff x;

    try
    {
        while (!node::shutdown)
        {
            x = {}; index = 0; BufferGuard b {x};
            erlang_msg msg;

            int got = ei_xreceive_msg_tmo (conn.fd, &msg, &x, 1000);

            if (got == ERL_MSG)
            {
                if (msg.msgtype == ERL_SEND)
                {
                }
                else if (msg.msgtype == ERL_REG_SEND)
                {
                    cout << CNODE << "received registered send, ignoring..." << endl;
                }
                else if (msg.msgtype == ERL_LINK)
                {
                    cout << CNODE << "parent process established link" << endl;
                }
                else if (msg.msgtype == ERL_UNLINK)
                {
                    throw runtime_error ("unlinked from parent process");
                }
                else if (msg.msgtype == ERL_EXIT)
                {
                    throw runtime_error ("parent process exit");
                }
            }
            else if (got == ERL_TICK)
            {
            }
            else if (got == ERL_ERROR)
            {
                if (erl_errno == ETIMEDOUT)
                {
                }
                else if (erl_errno == EAGAIN)
                {
                }
                else if (erl_errno == EMSGSIZE)
                {
                    throw runtime_error ("Msg size error during msg receive");
                }
                else if (erl_errno == EIO)
                {
                    throw runtime_error ("I/O error during msg receive");
                }
            }
        }
    }
    catch (runtime_error const& e)
    {
        cout << CNODE << "SHUTDOWN ERROR: " << e.what () << endl;
    }
    shutdown ();
}

void NodeConnection::outbound ()
{
    try
    {
        linkToRemote ();
        while (!node::shutdown)
        {
            this_thread::sleep_for (chrono::seconds {1});
        }
    }
    catch (runtime_error const& e)
    {
        cout << CNODE << "outbound error: " << e.what () << endl;
    }
    shutdown ();
}

void NodeConnection::linkToRemote ()
{
    erlang_pid* selfPid = ei_self (&conn.ec);
    selfPid->num = conn.fd;

    ei_x_buff x; BufferGuard b {x};

    ei_x_new_with_version (&x);
    ei_x_encode_tuple_header (&x, 2);
    ei_x_encode_atom (&x, "compute_node_started");
    ei_x_encode_pid (&x, selfPid);
    if (ei_reg_send (&conn.ec, conn.fd, (char*) VNN_UTILS, x.buff, x.index) < 0)
        throw runtime_error ("send error");
}
