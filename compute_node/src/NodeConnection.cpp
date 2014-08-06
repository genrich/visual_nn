#include <fstream>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

#include "NodeConnection.hpp"

using namespace std;

//--------------------------------------------------------------------------------------------------
namespace std
{
    template<> struct hash<NodeType> {
        std::size_t operator() (NodeType const& nodeType) const
        {
            return static_cast<std::size_t> (nodeType);
        }
    };
}

//--------------------------------------------------------------------------------------------------
std::string const& to_string (NodeType const& nodeType)
{
    static std::unordered_map<NodeType, const std::string> const names =
    {
        #define X(n) {NodeType::n, std::string {#n}}
        NODE_TYPES
        #undef X
    };
    return names.at (nodeType);
}

//--------------------------------------------------------------------------------------------------
string error_msg ()
{
    return string (strerror (erl_errno)) + " (" + to_string (erl_errno) + ")";
}

//--------------------------------------------------------------------------------------------------
string default_erlang_cookie ()
{
    string userHome (getenv ("HOME"));
    ifstream file {userHome + "/.erlang.cookie"};
    string cookie {istreambuf_iterator<char> {file}, istreambuf_iterator<char> {}};
    return cookie;
}

//--------------------------------------------------------------------------------------------------
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

//--------------------------------------------------------------------------------------------------
Connection::~Connection ()
{
    shutdown (fd, SHUT_RDWR);
    close (fd);
}

//--------------------------------------------------------------------------------------------------
NodeConnection::NodeConnection (string connectTo, string nodeName, string erlangCookie):
    conn {connectTo, nodeName, erlangCookie},
    inboundThread {&NodeConnection::inbound, this}
{
}

//--------------------------------------------------------------------------------------------------
NodeConnection::NodeConnection (): NodeConnection ("visual_nn@" + localhost (), "compute_node", default_erlang_cookie ())
{
}

//--------------------------------------------------------------------------------------------------
NodeConnection::~NodeConnection ()
{
    inboundThread.join ();
}

//--------------------------------------------------------------------------------------------------
void NodeConnection::inbound ()
{
    int version, arity, type, byteLength, index = 0;
    char atom[MAXATOMLEN];
    ei_x_buff x;

    try
    {
        linkToRemote ();

        while (true)
        {
            x = {}; index = 0; BufferGuard b {x};
            erlang_msg msg;

            int got = ei_xreceive_msg_tmo (conn.fd, &msg, &x, 1000);

            if (got == ERL_MSG)
            {
                if (msg.msgtype == ERL_SEND)
                {
                    if (ei_decode_version (x.buff, &index, &version))
                        throw runtime_error ("decoding version");

                    ei_get_type (x.buff, &index, &type, &byteLength);
                    if (type == ERL_ATOM_EXT)
                    {
                        if (ei_decode_atom (x.buff, &index, atom))
                            throw runtime_error ("decode atom");

                        string atomString {atom};

                        if (atomString == "create_network")
                        {
                            createNetwork ();
                        }
                    }
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
}

//--------------------------------------------------------------------------------------------------
void NodeConnection::linkToRemote ()
{
    erlang_pid* selfPid = ei_self (&conn.ec);
    selfPid->num = conn.fd;

    ei_x_buff x; BufferGuard b {x};

    ei_x_new_with_version (&x);
    ei_x_encode_tuple_header (&x, 2);
    ei_x_encode_atom (&x, "compute_node_started");
    ei_x_encode_pid (&x, selfPid);
    if (ei_reg_send (&conn.ec, conn.fd, (char*) VNN_CNODE, x.buff, x.index) < 0)
        throw runtime_error ("send error");
}

//--------------------------------------------------------------------------------------------------
void NodeConnection::createNetwork ()
{
    network.createStimulus ();
    network.createNetwork ();

    for (int i = 0; i < network.nodeTypes.size (); ++i)
    {
        sendAddNode (i, to_string (network.nodeTypes[i]), network.nodes[i*3], network.nodes[i*3 + 1], network.nodes[i*3 + 2]);
    }
}

//--------------------------------------------------------------------------------------------------
void NodeConnection::sendAddNode (int const id, string const type, float const x, float const y, float const z)
{
    ei_x_buff b; BufferGuard bg {b};
    ei_x_new_with_version (&b);
    ei_x_encode_tuple_header (&b, 2);
    ei_x_encode_atom (&b, "add_node");
    ei_x_encode_tuple_header (&b, 3);
    ei_x_encode_long (&b, id);
    ei_x_encode_atom (&b, type.c_str ());
    ei_x_encode_tuple_header (&b, 3);
    ei_x_encode_double (&b, x);
    ei_x_encode_double (&b, y);
    ei_x_encode_double (&b, z);
    if (ei_reg_send (&conn.ec, conn.fd, (char*) VNN_CNODE, b.buff, b.index) < 0)
        throw runtime_error ("send error");
}
