module kaleidic.nanomsg.nano;
import std.stdio;
import std.conv;
import std.string;

//nn.h
/*
    Copyright (c) 2012-2014 250bpm s.r.o.  All rights reserved.
    Copyright (c) 2013 GoPivotal, Inc.  All rights reserved.

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom
    the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
 */


/**
    Ported to Dlang (2014) by Laeeth Isharc.  Caveat emptor.

    Experimental more D-idiomatic interface added:
        struct nanomsg_t

 */

enum PROTO_SP      = 1;
enum SP_HDR        = 1;
enum NN_H_INCLUDED = 1;


/******************************************************************************/
/*  ABI versioning support.                                                   */
/******************************************************************************/

/*  Don't change this unless you know exactly what you're doing and have      */
/*  read and understand the following documents:                              */
/*  www.gnu.org/software/libtool/manual/html_node/Libtool-versioning.html     */
/*  www.gnu.org/software/libtool/manual/html_node/Updating-version-info.html  */

/*  The current interface version. */
enum
{
    NN_VERSION_CURRENT =2,
    NN_VERSION_REVISION=1,
    NN_VERSION_AGE     =2
}

/******************************************************************************/
/*  Errors.                                                                   */
/******************************************************************************/

/*  A number random enough not to collide with different errno ranges on      */
/*  different OSes. The assumption is that error_t is at least 32-bit type.   */
enum
{
    NN_HAUSNUMERO     =156384712,

    /*  On some platforms some standard POSIX errnos are not defined.    */
    ENOTSUP           =(NN_HAUSNUMERO + 1),
    EPROTONOSUPPORT   =(NN_HAUSNUMERO + 2),
    ENOBUFS           =(NN_HAUSNUMERO + 3),
    ENETDOWN          =(NN_HAUSNUMERO + 4),
    EADDRINUSE        =(NN_HAUSNUMERO + 5),
    EADDRNOTAVAIL     =(NN_HAUSNUMERO + 6),
    ECONNREFUSED      =(NN_HAUSNUMERO + 7),
    EINPROGRESS       =(NN_HAUSNUMERO + 8),
    ENOTSOCK          =(NN_HAUSNUMERO + 9),
    EAFNOSUPPORT      =(NN_HAUSNUMERO + 10),
    EPROTO            =(NN_HAUSNUMERO + 11),
    EAGAIN            =(NN_HAUSNUMERO + 12),
    EBADF             =(NN_HAUSNUMERO + 13),
    EINVAL            = (NN_HAUSNUMERO + 14),
    EMFILE            =(NN_HAUSNUMERO + 15),
    EFAULT            =(NN_HAUSNUMERO + 16),
    EACCESS           =(NN_HAUSNUMERO + 17),
    ENETRESET         =(NN_HAUSNUMERO + 18),
    ENETUNREACH       =(NN_HAUSNUMERO + 19),
    EHOSTUNREACH      = (NN_HAUSNUMERO + 20),
    ENOTCONN          =(NN_HAUSNUMERO + 21),
    EMSGSIZE          = (NN_HAUSNUMERO + 22),
    ETIMEDOUT         = (NN_HAUSNUMERO + 23),
    ECONNABORTED      = (NN_HAUSNUMERO + 24),
    ECONNRESET        =(NN_HAUSNUMERO + 25),
    ENOPROTOOPT       =(NN_HAUSNUMERO + 26),
    EISCONN           =(NN_HAUSNUMERO + 27),
    NN_EISCONN_DEFINED=1,
    ESOCKTNOSUPPORT   =(NN_HAUSNUMERO + 28),
    ETERM             =(NN_HAUSNUMERO + 53),
    EFSM              =(NN_HAUSNUMERO + 54),
}

/*  This function retrieves the errno as it is known to the library.          */
/*  The goal of this function is to make the code 100% portable, including    */
/*  where the library is compiled with certain CRT library (on Windows) and   */
/*  linked to an application that uses different CRT library.                 */
extern (C) int nn_errno();

/*  Resolves system errors and native errors to human-readable string.        */
extern (C) const(char *) nn_strerror(int errnum);


/*  Returns the symbol name (e.g. "NN_REQ") and value at a specified index.   */
/*  If the index is out-of-range, returns 0 and sets errno to EINVAL       */
/*  General usage is to start at i=0 and iterate until 0 is returned.      */
extern (C) char *nn_symbol(int i, int *value);

/*  Constants that are returned in `ns` member of nn_symbol_properties        */
enum NN_NS
{
    NAMESPACE        =0,
    VERSION          =1,
    DOMAIN           =2,
    TRANSPORT        = 3,
    PROTOCOL         =4,
    OPTION_LEVEL     = 5,
    SOCKET_OPTION    = 6,
    TRANSPORT_OPTION =7,
    OPTION_TYPE      =8,
    OPTION_UNIT      =9,
    FLAG             =10,
    ERROR            =11,
    LIMIT            =12,
}
enum NN_TYPE
{
    NONE=0,
    INT =1,
    STR =2,
}
enum NN_UNIT
{
    NONE         =0,
    BYTES        =1,
    MILLISECONDS =2,
    PRIORITY     =3,
    BOOLEAN      =4,
}


align (1) struct nn_symbol_properties
{
    int        value;
    const char *name;
    int        ns;
    int        type;
    int        unit;
};

extern (C) int nn_symbol_info(int i, nn_symbol_properties *buf, int buflen);
extern (C) void nn_term();


enum NN_MSG = -1;


extern (C) void *nn_allocmsg(size_t size, int type);
extern (C) void *nn_reallocmsg(void *msg, size_t size);
extern (C) int nn_freemsg(void *msg);

struct nn_iovec
{
    void   *iov_base;
    size_t iov_len;
};

struct nn_msghdr
{
    nn_iovec *msg_iov;
    int      msg_iovlen;
    void     *msg_control;
    size_t   msg_controllen;
};

struct nn_cmsghdr
{
    size_t cmsg_len;
    int    cmsg_level;
    int    cmsg_type;
};

/*  Internal function. Not to be used directly.                               */
/*  Use NN_CMSG_NEXTHDR macro instead.                                        */
static extern (C)nn_cmsghdr * nn_cmsg_nexthdr_(const nn_msghdr * mhdr, const nn_cmsghdr * cmsg)
{
    size_t sz;

    sz = nn_cmsghdr.sizeof + cmsg.cmsg_len;
    if ((cast(char *)cmsg) - (cast(char *)mhdr.msg_control) + sz >= mhdr.msg_controllen)
        return cast(nn_cmsghdr *)0;

    return cast(nn_cmsghdr *)((cast(char *)cmsg) + sz);
}

T NN_CMSG_FIRSTHDR(T) (T mhdr)
{
    return (mhdr.msg_controllen >= sizeof(nn_cmsghdr)) ? cast(nn_cmsghdr *)mhdr.msg_control : cast(nn_cmsghdr *)0;
}
T  NN_CMSG_NXTHDR(T, U) (T mhdr, U cmsg)
{
    return nn_cmsg_nexthdr_(cast(nn_msghdr *)(mhdr), cast(nn_cmsghdr *)(cmsg));
}

T  NN_CMSG_DATA(T) (T cmsg)
{
    return(cast(ubyte *)((cast( nn_cmsghdr *)(cmsg)) + 1));
}

/*  Helper macro. Not to be used directly.                                    */
T NN_CMSG_ALIGN(T) (T len)
{
    return(((len) + sizeof(size_t) - 1) & (size_t) ~(sizeof(size_t) - 1));
}

/* Extensions to POSIX defined by RFC3542.                                    */

T  NN_CMSG_SPACE(T) (T len)
{
    return(NN_CMSG_ALIGN(len) + NN_CMSG_ALIGN(sizeof(nn_cmsghdr)));
}

T  NN_CMSG_LEN(T) (T len)
{
    return(NN_CMSG_ALIGN(sizeof(nn_cmsghdr)) + (len));
}

/*  SP address families.                                                      */
enum AF_SP     = 1;
enum AF_SP_RAW = 2;

/*  Max size of an SP address.                                                */
enum NN_SOCKADDR_MAX = 128;

/*  Socket option levels: Negative numbers are reserved for transports,
    positive for socket types. */
enum NN_SOL_SOCKET = 0;

/*  Generic socket options (NN_SOL_SOCKET level).                             */
enum NN
{
    LINGER            =1,
    SNDBUF            =2,
    RCVBUF            =3,
    SNDTIMEO          =4,
    RCVTIMEO          =5,
    RECONNECT_IVL     = 6,
    RECONNECT_IVL_MAX =7,
    SNDPRIO           =8,
    RCVPRIO           = 9,
    RCVFD             =11,
    DOMAIN            =12,
    ROTOCOL           =13,
    IPV4ONLY          =14,
    SOCKET_NAME       =15,
}

/*  Send/recv options.                                                        */
enum NN_DONTWAIT      = 1;

extern (C) int nn_socket(int domain, int protocol);
extern (C) int nn_close(int s);
extern (C) int nn_setsockopt(int s, int level, int option, const void *optval, size_t optvallen);
extern (C) int nn_getsockopt(int s, int level, int option, void *optval, size_t *optvallen);
extern (C) int nn_bind(int s, const char *addr);
extern (C) int nn_connect(int s, const char *addr);
extern (C) int nn_shutdown(int s, int how);
extern (C) int nn_send(int s, const void *buf, size_t len, int flags);
extern (C) int nn_recv(int s, void *buf, size_t len, int flags);
extern (C) int nn_sendmsg(int s, const nn_msghdr *msghdr, int flags);
extern (C) int nn_recvmsg(int s, nn_msghdr *msghdr, int flags);



enum NN_POLLIN  = 1;
enum NN_POLLOUT = 2;

struct nn_pollfd
{
    int   fd;
    short events;
    short revents;
};

extern (C) int nn_poll(nn_pollfd *fds, int nfds, int timeout);
extern (C) int nn_device(int s1, int s2);


enum PAIR_H_INCLUDED = 1;
enum NN_PROTO_PAIR   = 1;

enum NN_PAIR              = (NN_PROTO_PAIR * 16 + 0);
enum PIPELINE_H_INCLUDED  = 1;
enum NN_PROTO_PIPELINE    = 5;
enum NN_PUSH              = (NN_PROTO_PIPELINE * 16 + 0);
enum NN_PULL              = (NN_PROTO_PIPELINE * 16 + 1);
enum NN_PROTOCOL_INCLUDED = 1;
struct nn_ctx;
enum NN_PIPE
{
    RELEASE=1,
    PARSED =2,
    IN     =33987,
    OUT    =33988,
}
struct nn_pipe;
struct nn_msg;

extern (C)
{
void nn_pipe_setdata(nn_pipe *self, void *data);
void *nn_pipe_getdata(nn_pipe *self);
int nn_pipe_send(nn_pipe *self, nn_msg *msg);
int nn_pipe_recv(nn_pipe *self, nn_msg *msg);
void nn_pipe_getopt(nn_pipe *self, int level, int option, void *optval, size_t *optvallen);
}


/******************************************************************************/
/*  Base class for all socket types.                                          */
/******************************************************************************/

/*  Any combination of these events can be returned from 'events' virtual
    function. */
enum  NN_SOCKBASE_EVENT_IN = 1;
enum NN_SOCKBASE_EVENT_OUT = 2;

/*  To be implemented by individual socket types. */
struct nn_sockbase_vfptr
{
    void function(nn_sockbase *) stop;
    void function(nn_sockbase *) destroy;
    int function(nn_sockbase *, nn_pipe *) add;
    void function(nn_sockbase *, nn_pipe *) rm;
    void function(nn_sockbase *, nn_pipe *) IN;
    void function(nn_sockbase *, nn_pipe *) OUT;
    int function(nn_sockbase *) events;
    int function(nn_sockbase *, nn_msg *) send;
    int function(nn_sockbase *, nn_msg *) recv;
    int function(nn_sockbase *, int level, int option, const void *optval, size_t optvallen) setopt;
    int function(nn_sockbase *, int level, int option, void *optval, size_t *optvallen) getopt;
};

struct nn_sockbase
{
    const nn_sockbase_vfptr *vfptr;
    nn_sock                 *sock;
};

/*  Initialise the socket base class. 'hint' is the opaque value passed to the
    nn_transport's 'create' function. */
extern (C)
void nn_sockbase_init(nn_sockbase *self, const nn_sockbase_vfptr *vfptr, void *hint);

/*  Terminate the socket base class. */
extern (C)
void nn_sockbase_term(nn_sockbase *self);

/*  Call this function when stopping is done. */
extern (C)
void nn_sockbase_stopped(nn_sockbase *self);

/*  Returns the AIO context associated with the socket. This function is
    useful when socket type implementation needs to create async objects,
    such as timers. */
extern (C)
nn_ctx * nn_sockbase_getctx(nn_sockbase * self);

/*  Retrieve a NN_SOL_SOCKET-level option. */
extern (C)
int nn_sockbase_getopt(nn_sockbase *self, int option, void *optval, size_t *optvallen);

/*  Add some statitistics for socket  */
extern (C)
void nn_sockbase_stat_increment(nn_sockbase *self, int name, int increment);

enum NN_STAT_CURRENT_SND_PRIORITY = 401;

/******************************************************************************/
/*  The socktype class.                                                       */
/******************************************************************************/

/*  This structure defines a class factory for individual socket types. */

/*  Specifies that the socket type can be never used to receive messages. */
enum NN_SOCKTYPE_FLAG_NORECV = 1;

/*  Specifies that the socket type can be never used to send messages. */
enum NN_SOCKTYPE_FLAG_NOSEND = 2;

struct nn_socktype
{
    int          domain;
    int          protocol;
    int          flags;
    int function(void *hint, nn_sockbase **sockbase) create;
    int function(int socktype) ispeer;
    nn_list_item *item;
};



enum NN_PROTO_PUBSUB = 2;
enum NN_PUB          = NN_PROTO_PUBSUB * 16 + 0;
enum NN_SUB          = NN_PROTO_PUBSUB * 16 + 1;

enum NN_PROTO_BUS = 7;
enum NN_BUS       = (NN_PROTO_BUS * 16 + 0);

enum NN_INPROC = -1;
enum NN_IPC    = -2;


align (1) union nn_req_handle
{
    int  i;
    void *ptr;
};

extern (C) int nn_req_send(int s, nn_req_handle hndl, const void *buf, size_t len, int flags);
extern (C) int nn_req_recv(int s, nn_req_handle *hndl, void *buf, size_t len, int flags);


enum NN_SUB_SUBSCRIBE   = 1;
enum NN_SUB_UNSUBSCRIBE = 2;
enum REQREP_H_INCLUDED  = 1;
enum NN_PROTO_REQREP    = 3;

enum NN_REQ = NN_PROTO_REQREP * 16 + 0;
enum NN_REP = NN_PROTO_REQREP * 16 + 1;

enum NN_REQ_RESEND_IVL = 1;
enum SURVEY_H_INCLUDED = 1;
enum NN_PROTO_SURVEY   = 6;

enum NN_SURVEYOR   = (NN_PROTO_SURVEY * 16 + 2);
enum NN_RESPONDENT = (NN_PROTO_SURVEY * 16 + 3);

enum NN_SURVEYOR_DEADLINE = 1;
enum TCP_H_INCLUDED       = 1;
enum NN_TCP               = -3;
enum NN_TCP_NODELAY       = 1;
struct nn_sock;
struct nn_cp;
struct nn_ep;
struct nn_optset_vfptr
{
    extern (C) void function(nn_optset *self) destroy;
    extern (C) int function(nn_optset *self, int option, const void *optval, size_t optvallen) setopt;
    extern (C) int function(nn_optset *self, int option, void *optval, size_t *optvallen) getopt;
};

struct nn_optset
{
    const nn_optset_vfptr *vfptr;
};


struct nn_epbase_vfptr
{
    extern (C) void function(nn_epbase *) stop;
    extern (C) void function(nn_epbase *) destroy;
};

struct nn_epbase
{
    const nn_epbase_vfptr *vfptr;
    nn_ep                 *ep;
};

/*  Creates a new endpoint. 'hint' parameter is an opaque value that
    was passed to transport's bind or connect function. */
extern (C)
{
void nn_epbase_init(nn_epbase *self, const nn_epbase_vfptr *vfptr, void *hint);
void nn_epbase_stopped(nn_epbase *self);
void nn_epbase_term(nn_epbase *self);
nn_ctx *nn_epbase_getctx(nn_epbase *self);
char *nn_epbase_getaddr(nn_epbase *self);
void nn_epbase_getopt(nn_epbase *self, int level, int option, void *optval, size_t *optvallen);
int nn_epbase_ispeer(nn_epbase *self, int socktype);
void nn_epbase_set_error(nn_epbase *self, int errnum);
void nn_epbase_clear_error(nn_epbase *self);
void nn_epbase_stat_increment(nn_epbase *self, int name, int increment);
}

enum NN_STAT
{
    ESTABLISHED_CONNECTIONS=101,
    ACCEPTED_CONNECTIONS   =102,
    DROPPED_CONNECTIONS    =103,
    BROKEN_CONNECTIONS     =104,
    CONNECT_ERRORS         =105,
    BIND_ERRORS            =106,
    ACCEPT_ERRORS          =107,
    CURRENT_CONNECTIONS    =201,
    INPROGRESS_CONNECTIONS =202,
    CURRENT_EP_ERRORS      =203,
};
enum NN_PIPEBASE
{
    RELEASE=1,
    PARSED =2,
};

struct nn_pipebase_vfptr
{
    alias _send = int function(nn_pipebase *self, nn_msg *msg);
    alias _recv = int function(nn_pipebase *self, nn_msg *msg);
    _send send;
    _recv recv;
};
struct nn_ep_options
{
    int sndprio;
    int rcvprio;
    int ipv4only;
};
struct nn_pipebase
{
    nn_fsm                  fsm;
    const nn_pipebase_vfptr *vfptr;
    ubyte                   state;
    ubyte                   instate;
    ubyte                   outstate;
    nn_sock                 *sock;
    void                    *data;
    nn_fsm_event            IN;
    nn_fsm_event            OUT;
    nn_ep_options           options;
};
extern (C)
{
void nn_pipebase_init(nn_pipebase *, const nn_pipebase_vfptr *vfptr, nn_epbase *epbase);
void nn_pipebase_term(nn_pipebase *);
int nn_pipebase_start(nn_pipebase *);
void nn_pipebase_stop(nn_pipebase *);
void nn_pipebase_received(nn_pipebase *);
void nn_pipebase_sent(nn_pipebase *);
void nn_pipebase_getopt(nn_pipebase *, int level, int option, void *optval, size_t *optvallen);
int nn_pipebase_ispeer(nn_pipebase *, int socktype);
}

struct nn_transport
{
    const char *name;
    int        id;
    extern (C) void function() init;
    extern (C) void function() term;
    extern (C) int function(void *hint, nn_epbase **epbase) bind;
    extern (C) int function(void *hint, nn_epbase **epbase) connect;
    nn_optset *function() optset;
    nn_list_item          item;
};

enum NN_FSM_INCLUDED = 1;

struct nn_worker;

struct nn_fsm_event
{
    nn_fsm        *fsm;
    int           src;
    void          *srcptr;
    int           type;
    nn_queue_item item;
};

extern (C)
{
void nn_fsm_event_init(nn_fsm_event *self);
void nn_fsm_event_term(nn_fsm_event *self);
int nn_fsm_event_active(nn_fsm_event *self);
void nn_fsm_event_process(nn_fsm_event *self);
}

/*  Special source for actions. It's negative not to clash with user-defined
    sources. */
enum NN_FSM_ACTION = -2;

/*  Actions generated by fsm object. The values are negative not to clash
    with user-defined actions. */
enum NN_FSM_START = -2;
enum NN_FSM_STOP  = -3;

/*  Virtual function to be implemented by the derived class to handle the
    incoming events. */
alias nn_fsm_fn = void function(nn_fsm *, int, int, void *);

struct nn_fsm_owner
{
    int    src;
    nn_fsm *fsm;
};

struct nn_fsm
{
    nn_fsm_fn    fn;
    nn_fsm_fn    shutdown_fn;
    int          state;
    int          src;
    void         *srcptr;
    nn_fsm       *owner;
    nn_ctx       *ctx;
    nn_fsm_event stopped;
};

extern (C)
{
void nn_fsm_init_root(nn_fsm *self, nn_fsm_fn fn, nn_fsm_fn shutdown_fn, nn_ctx *ctx);
void nn_fsm_init(nn_fsm *, nn_fsm_fn fn, nn_fsm_fn shutdown_fn, int src, void *srcptr, nn_fsm *owner);
void nn_fsm_term(nn_fsm *);
int nn_fsm_isidle(nn_fsm *);
void nn_fsm_start(nn_fsm *);
void nn_fsm_stop(nn_fsm *);
void nn_fsm_stopped(nn_fsm *, int type);
void nn_fsm_stopped_noevent(nn_fsm *);
void nn_fsm_swap_owner(nn_fsm *, nn_fsm_owner *owner);
nn_worker *nn_fsm_choose_worker(nn_fsm *);
void nn_fsm_action(nn_fsm *, int type);
void nn_fsm_raise(nn_fsm *, nn_fsm_event *event, int type);
void nn_fsm_raiseto(nn_fsm *, nn_fsm *dst, nn_fsm_event *event, int src, int type, void *srcptr);
void nn_fsm_feed(nn_fsm *, int src, int type, void *srcptr);
}

enum NN_LIST_INCLUDED = 1;

struct nn_list_item
{
    nn_list_item *next;
    nn_list_item *prev;
};

struct nn_list
{
    nn_list_item *first;
    nn_list_item *last;
};

/*  Undefined value for initializing a list item which is not part of a list. */
const nn_list_item *NN_LIST_NOTINLIST = cast(const nn_list_item *)-1;

/*  Use for initializing a list item statically. */
auto NN_LIST_ITEM_INITIALIZER = [ NN_LIST_NOTINLIST, NN_LIST_NOTINLIST ];


/*  Initialise the list. */
extern (C)
{
void nn_list_init(nn_list *);
void nn_list_term(nn_list *);
int nn_list_empty(nn_list *);
nn_list_item *nn_list_begin(nn_list *);
nn_list_item *nn_list_end(nn_list *);
nn_list_item *nn_list_prev(nn_list *, nn_list_item *);
nn_list_item *nn_list_next(nn_list *, nn_list_item *);
void nn_list_insert(nn_list *, nn_list_item *, nn_list_item *);
nn_list_item *nn_list_erase(nn_list *, nn_list_item *);
void nn_list_item_init(nn_list_item *);
void nn_list_item_term(nn_list_item *);
int nn_list_item_isinlist(nn_list_item *);
}

enum NN_QUEUE_INCLUDED = 1;

/*  Undefined value for initialising a queue item which is not
    part of a queue. */
const nn_queue_item *NN_QUEUE_NOTINQUEUE = cast(const nn_queue_item *)-1;


/+
   /*  Use for initialising a queue item statically. */
   auto NN_QUEUE_ITEM_INITIALIZER()
   {
    return [NN_LIST_NOTINQUEUE];
   }
 +/

struct nn_queue_item
{
    nn_queue_item *next;
};

struct nn_queue
{
    nn_queue_item *head;
    nn_queue_item *tail;
};
extern (C)
{
void nn_queue_init(nn_queue *);
void nn_queue_term(nn_queue *);
int nn_queue_empty(nn_queue *);
void nn_queue_push(nn_queue *, nn_queue_item *);
nn_queue_item *nn_queue_pop(nn_queue *);
void nn_queue_item_init(nn_queue_item *);
void nn_queue_item_term(nn_queue_item *);
int nn_queue_item_isinqueue(nn_queue_item *);
}


struct nanomsg_t
{
    char   *url;
    int    sock       = -1;
    char   *buf       = cast(char *)0;
    bool   isshutdown = true;

    string surl() ()
    {
        return to!string(url);
    }
    
//    this(int param1 = AF_SP, int param2 = NN_REP)
    this(int param1, int param2)
    {
        sock = nn_socket(param1, param2);
        if (sock < 0)
            throw new Exception("cannot create nanomsg socket for modes " ~ to!string(param1) ~ " " ~ to!string(param2));
        isshutdown = false;
    }

    void open(string surl, bool bind = true)
    {
        if (sock < 0)
            throw new Exception("cannot create nanomsg socket for " ~ surl);
        if (bind)
        {
            if (nn_bind(sock, toStringz(surl)) < 0)
                throw new Exception("nanomsg did not bind to new socket for " ~ surl);
        }
        else
        {
            if (nn_connect(sock, toStringz(surl)) < 0)
                throw new Exception("nanomsg did not connect to new socket for " ~ surl);
        }
    }
    ubyte[] recv(int flags)
    {
        ubyte[] recvbytes;
        buf = cast(char *)0;
        //consider returning as sized array without copy
        auto numbytes = nn_recv(sock, &buf, NN_MSG, flags);
        if (numbytes >= 0)
        {
            recvbytes.length = numbytes + 1;
            foreach (i; 0..numbytes)
            {
                recvbytes[ i ] = buf[ i ];
            }
            if (buf)
            {
                nn_freemsg(buf);
                buf = cast(char *)0;
            }
            return recvbytes;
        }
        else
        {
            if (buf)
            {
                nn_freemsg(buf);
                buf = cast(char *)0;
            }
            throw new Exception("nanomsg encountered an error whilst trying to receive a message for " ~ to!string(url) ~ " error:" ~ to!string(
                                                                                                                                                nn_strerror(
                                                                                                                                                            nn_errno())));
        }
    }

    string recv_as_string(int param1 = NN_MSG)
    {
        return to!string(recv(param1));
    }

    int send(char *mybuf, int numbytes)
    {
        return nn_send(sock, mybuf, numbytes, 0);
    }

    int send(ubyte[] mybuf)
    {
        return nn_send(sock, cast(char *)mybuf, mybuf.length + 1, 0);
    }

    int send(string mybuf)
    {
        return nn_send(sock, cast(char *)mybuf, cast(int)mybuf.length + 1, 0);
    }

    void setopt(T) (int level, int option, T optval)
    {
        nn_setsockopt(sock, level, option, optval, (*optval).size);
    }

    void getopt(int level, int option, void *optval, size_t *optvallen)
    {
        nn_getsockopt(sock, level, option, optval, optvallen);
    }
    void close()
    {
        nn_close(sock);
    }

    int sendmsg(const nn_msghdr *msghdr, int flags)
    {
        return nn_sendmsg(sock, msghdr, flags);
    }

    int recvmsg(nn_msghdr *msghdr, int flags)
    {
        return nn_recvmsg(sock, msghdr, flags);
    }

    bool canreceive()
    {
        nn_pollfd pfd;

        pfd.fd     = sock;
        pfd.events = NN_POLLIN;
        auto res = nn_poll(&pfd, 1, 2000);
        if (res == -1)
            throw new Exception("unable to check socket readiness status - res=" ~ to!string(res) ~ "errno=" ~ to!string(nn_errno()));
        return (pfd.revents && NN_POLLIN) != 0;
    }

    bool cansend()
    {
        nn_pollfd pfd;

        pfd.fd     = sock;
        pfd.events = NN_POLLOUT;
        auto res = nn_poll(&pfd, 1, 2000);
        if (res == -1)
            throw new Exception("unable to check socket readiness status");
        return (pfd.revents && NN_POLLOUT) != 0;
    }

    void freemsg()
    {
        if (buf)
            nn_freemsg(buf);
        buf = cast(char *)0;
    }

    void shutdown(int param1)
    {
        if (buf)
            nn_freemsg(buf);
        auto ret = nn_shutdown(sock, param1); // we should check this value and throw exception if need be
        sock       = -1;
        isshutdown = true;
    }

    ~this()
    {
        if (!isshutdown)
            shutdown(0);
    }

    int errcheck(int retval)
    {
        if (retval == -1)
            throw new Exception("nanomsg error: ");
        else
            return retval;
    }
}






/*  Returns the symbol name (e.g. "NN_REQ") and value at a specified index.   */
/*  If the index is out-of-range, returns null and sets errno to EINVAL       */
/*  General usage is to start at i=0 and iterate until null is returned.      */
extern (C) const(char *) nn_symbol(int i, int *value);


/*  Constants that are returned in `type` member of nn_symbol_properties      */
enum NN_TYPE_NONE = 0;
enum NN_TYPE_INT  = 1;
enum NN_TYPE_STR  = 2;

/*  Constants that are returned in the `unit` member of nn_symbol_properties  */
enum NN_UNIT_NONE         = 0;
enum NN_UNIT_BYTES        = 1;
enum NN_UNIT_MILLISECONDS = 2;
enum NN_UNIT_PRIORITY     = 3;
enum NN_UNIT_BOOLEAN      = 4;


/*  Internal stuff. Not to be used directly.                                  */
extern (C)nn_cmsghdr * nn_cmsg_nxthdr_(const nn_msghdr * mhdr, const nn_cmsghdr * cmsg);

auto NN_CMSG_ALIGN_(size_t len)
{
    return(((len) + size_t.sizeof - 1) & (~(size_t.sizeof - 1)));
}

/*auto NN_CMSG_FIRSTHDR(mhdr)
   {
    nn_cmsg_nxthdr_ (cast(nn_msghdr*) (mhdr), null);
   }

   enum NN_CMSG_NXTHDR(mhdr, cmsg) \
    nn_cmsg_nxthdr_ ((struct nn_msghdr*) (mhdr), (struct nn_cmsghdr*) (cmsg))

   enum NN_CMSG_DATA(cmsg) \
    ((unsigned char*) (((struct nn_cmsghdr*) (cmsg)) + 1))

   Extensions to POSIX defined by RFC 3542.                                   */

auto NN_CMSG_SPACE(size_t len)
{
    return(NN_CMSG_ALIGN_(len) + NN_CMSG_ALIGN_(nn_cmsghdr.sizeof));
}

/*enum NN_CMSG_LEN(len) \
    (NN_CMSG_ALIGN_ (sizeof (struct nn_cmsghdr)) + (len))
 */


/+
   #define NN_PIPE_RELEASE 1

   /*  Specifies that received message is already split into header and body.
    This flag is used only by inproc transport to avoid merging and re-splitting
    the messages passed with a single process. */
   #define NN_PIPE_PARSED 2

   /*  Events generated by the pipe. */
   #define NN_PIPE_IN 33987
   #define NN_PIPE_OUT 33988

   struct nn_pipe;

   /*  Associates opaque pointer to protocol-specific data with the pipe. */
   void nn_pipe_setdata (struct nn_pipe *self, void *data);

   /*  Retrieves the opaque pointer associated with the pipe. */
   void *nn_pipe_getdata (struct nn_pipe *self);

   /*  Send the message to the pipe. If successful, pipe takes ownership of the
    messages. */
   int nn_pipe_send (struct nn_pipe *self, struct nn_msg *msg);

   /*  Receive a message from a pipe. 'msg' should not be initialised prior to
    the call. It will be initialised when the call succeeds. */
   int nn_pipe_recv (struct nn_pipe *self, struct nn_msg *msg);

   /*  Get option for pipe. Mostly useful for endpoint-specific options  */
   void nn_pipe_getopt (struct nn_pipe *self, int level, int option,
    void *optval, size_t *optvallen);


   /******************************************************************************/
   /*  Base class for all socket types.                                          */
   /******************************************************************************/

   struct nn_sockbase;

   /*  Any combination of these events can be returned from 'events' virtual
    function. */
   #define NN_SOCKBASE_EVENT_IN 1
   #define NN_SOCKBASE_EVENT_OUT 2

   /*  To be implemented by individual socket types. */
   struct nn_sockbase_vfptr {

    /*  Ask socket to stop. */
    void (*stop) (struct nn_sockbase *self);

    /*  Deallocate the socket. */
    void (*destroy) (struct nn_sockbase *self);

    /*  Management of pipes. 'add' registers a new pipe. The pipe cannot be used
        to send to or to be received from at the moment. 'rm' unregisters the
        pipe. The pipe should not be used after this call as it may already be
        deallocated. 'in' informs the socket that pipe is readable. 'out'
        informs it that it is writable. */
    int (*add) (struct nn_sockbase *self, struct nn_pipe *pipe);
    void (*rm) (struct nn_sockbase *self, struct nn_pipe *pipe);
    void (*in) (struct nn_sockbase *self, struct nn_pipe *pipe);
    void (*out) (struct nn_sockbase *self, struct nn_pipe *pipe);

    /*  Return any combination of event flags defined above, thus specifying
        whether the socket should be readable, writable, both or none. */
    int (*events) (struct nn_sockbase *self);

    /*  Send a message to the socket. Returns -EAGAIN if it cannot be done at
        the moment or zero in case of success. */
    int (*send) (struct nn_sockbase *self, struct nn_msg *msg);

    /*  Receive a message from the socket. Returns -EAGAIN if it cannot be done
        at the moment or zero in case of success. */
    int (*recv) (struct nn_sockbase *self, struct nn_msg *msg);

    /*  Set a protocol specific option. */
    int (*setopt) (struct nn_sockbase *self, int level, int option,
        const void *optval, size_t optvallen);

    /*  Retrieve a protocol specific option. */
    int (*getopt) (struct nn_sockbase *self, int level, int option,
        void *optval, size_t *optvallen);
   };

   struct nn_sockbase {
    const struct nn_sockbase_vfptr *vfptr;
    struct nn_sock *sock;
   };

   /*  Initialise the socket base class. 'hint' is the opaque value passed to the
    nn_transport's 'create' function. */
   void nn_sockbase_init (struct nn_sockbase *self,
    const struct nn_sockbase_vfptr *vfptr, void *hint);

   /*  Terminate the socket base class. */
   void nn_sockbase_term (struct nn_sockbase *self);

   /*  Call this function when stopping is done. */
   void nn_sockbase_stopped (struct nn_sockbase *self);

   /*  Returns the AIO context associated with the socket. This function is
    useful when socket type implementation needs to create async objects,
    such as timers. */
   struct nn_ctx *nn_sockbase_getctx (struct nn_sockbase *self);

   /*  Retrieve a NN_SOL_SOCKET-level option. */
   int nn_sockbase_getopt (struct nn_sockbase *self, int option,
    void *optval, size_t *optvallen);

   /*  Add some statistics for socket  */
   void nn_sockbase_stat_increment (struct nn_sockbase *self, int name,
    int increment);

   #define NN_STAT_CURRENT_SND_PRIORITY 401

   /******************************************************************************/
   /*  The socktype class.                                                       */
   /******************************************************************************/

   /*  This structure defines a class factory for individual socket types. */

   /*  Specifies that the socket type can be never used to receive messages. */
   #define NN_SOCKTYPE_FLAG_NORECV 1

   /*  Specifies that the socket type can be never used to send messages. */
   #define NN_SOCKTYPE_FLAG_NOSEND 2

   struct nn_socktype {

    /*  Domain and protocol IDs as specified in nn_socket() function. */
    int domain;
    int protocol;

    /*  Any combination of the flags defined above. */
    int flags;

    /*  Function to create specific socket type. 'sockbase' is the output
        parameter to return reference to newly created socket. This function
        is called under global lock, so it is not possible that two sockets are
        being created in parallel. */
    int (*create) (void *hint, struct nn_sockbase **sockbase);

    /*  Returns 1 if the supplied socket type is a valid peer for this socket,
        0 otherwise. Note that the validation is done only within a single
        SP protocol. Peers speaking other SP protocols are discarded by the
        core and socket is not even asked to validate them. */
    int (*ispeer) (int socktype);

    /*  This member is owned by the core. Never touch it directly from inside
        the protocol implementation. */
    struct nn_list_item item;
   };


   /*  This is the API between the nanomsg core and individual transports. */

   struct nn_sock;
   struct nn_cp;

   /******************************************************************************/
   /*  Container for transport-specific socket options.                          */
   /******************************************************************************/

   struct nn_optset;

   struct nn_optset_vfptr {
    void (*destroy) (struct nn_optset *self);
    int (*setopt) (struct nn_optset *self, int option, const void *optval,
        size_t optvallen);
    int (*getopt) (struct nn_optset *self, int option, void *optval,
        size_t *optvallen);
   };

   struct nn_optset {
    const struct nn_optset_vfptr *vfptr;
   };

   /******************************************************************************/
   /*  The base class for endpoints.                                             */
   /******************************************************************************/

   /*  The best way to think about endpoints is that endpoint is an object created
    by each nn_bind() or nn_connect() call. Each endpoint is associated with
    exactly one address string (e.g. "tcp://127.0.0.1:5555"). */

   struct nn_epbase;

   struct nn_epbase_vfptr {

    /*  Ask the endpoint to stop itself. The endpoint is allowed to linger
        to send the pending outbound data. When done, it reports the fact by
        invoking nn_epbase_stopped() function. */
    void (*stop) (struct nn_epbase *self);

    /*  Deallocate the endpoint object. */
    void (*destroy) (struct nn_epbase *self);
   };

   struct nn_epbase {
    const struct nn_epbase_vfptr *vfptr;
    struct nn_ep *ep;
   };

   /*  Creates a new endpoint. 'hint' parameter is an opaque value that
    was passed to transport's bind or connect function. */
   void nn_epbase_init (struct nn_epbase *self,
    const struct nn_epbase_vfptr *vfptr, void *hint);

   /*  Notify the user that stopping is done. */
   void nn_epbase_stopped (struct nn_epbase *self);

   /*  Terminate the epbase object. */
   void nn_epbase_term (struct nn_epbase *self);

   /*  Returns the AIO context associated with the endpoint. */
   struct nn_ctx *nn_epbase_getctx (struct nn_epbase *self);

   /*  Returns the address string associated with this endpoint. */
   const char *nn_epbase_getaddr (struct nn_epbase *self);

   /*  Retrieve value of a socket option. */
   void nn_epbase_getopt (struct nn_epbase *self, int level, int option,
    void *optval, size_t *optvallen);

   /*  Returns 1 is the specified socket type is a valid peer for this socket,
    or 0 otherwise. */
   int nn_epbase_ispeer (struct nn_epbase *self, int socktype);

   /*  Notifies a monitoring system the error on this endpoint  */
   void nn_epbase_set_error(struct nn_epbase *self, int errnum);

   /*  Notifies a monitoring system that error is gone  */
   void nn_epbase_clear_error(struct nn_epbase *self);

   /*  Increments statistics counters in the socket structure  */
   void nn_epbase_stat_increment(struct nn_epbase *self, int name, int increment);


   #define NN_STAT_ESTABLISHED_CONNECTIONS 101
   #define NN_STAT_ACCEPTED_CONNECTIONS    102
   #define NN_STAT_DROPPED_CONNECTIONS     103
   #define NN_STAT_BROKEN_CONNECTIONS      104
   #define NN_STAT_CONNECT_ERRORS          105
   #define NN_STAT_BIND_ERRORS             106
   #define NN_STAT_ACCEPT_ERRORS           107

   #define NN_STAT_CURRENT_CONNECTIONS     201
   #define NN_STAT_INPROGRESS_CONNECTIONS  202
   #define NN_STAT_CURRENT_EP_ERRORS       203


   /******************************************************************************/
   /*  The base class for pipes.                                                 */
   /******************************************************************************/

   /*  Pipe represents one "connection", i.e. perfectly ordered uni- or
    bi-directional stream of messages. One endpoint can create multiple pipes
    (for example, bound TCP socket is an endpoint, individual accepted TCP
    connections are represented by pipes. */

   struct nn_pipebase;

   /*  This value is returned by pipe's send and recv functions to signalise that
    more sends/recvs are not possible at the moment. From that moment on,
    the core will stop invoking the function. To re-establish the message
    flow nn_pipebase_received (respectively nn_pipebase_sent) should
    be called. */
   #define NN_PIPEBASE_RELEASE 1

   /*  Specifies that received message is already split into header and body.
    This flag is used only by inproc transport to avoid merging and re-splitting
    the messages passed with a single process. */
   #define NN_PIPEBASE_PARSED 2

   struct nn_pipebase_vfptr {

    /*  Send a message to the network. The function can return either error
        (negative number) or any combination of the flags defined above. */
    int (*send) (struct nn_pipebase *self, struct nn_msg *msg);

    /*  Receive a message from the network. The function can return either error
        (negative number) or any combination of the flags defined above. */
    int (*recv) (struct nn_pipebase *self, struct nn_msg *msg);
   };

   /*  Endpoint specific options. Same restrictions as for nn_pipebase apply  */
   struct nn_ep_options
   {
    int sndprio;
    int rcvprio;
    int ipv4only;
   };

   /*  The member of this structure are used internally by the core. Never use
    or modify them directly from the transport. */
   struct nn_pipebase {
    struct nn_fsm fsm;
    const struct nn_pipebase_vfptr *vfptr;
    uint8_t state;
    uint8_t instate;
    uint8_t outstate;
    struct nn_sock *sock;
    void *data;
    struct nn_fsm_event in;
    struct nn_fsm_event out;
    struct nn_ep_options options;
   };

   /*  Initialise the pipe.  */
   void nn_pipebase_init (struct nn_pipebase *self,
    const struct nn_pipebase_vfptr *vfptr, struct nn_epbase *epbase);

   /*  Terminate the pipe. */
   void nn_pipebase_term (struct nn_pipebase *self);

   /*  Call this function once the connection is established. */
   int nn_pipebase_start (struct nn_pipebase *self);

   /*  Call this function once the connection is broken. */
   void nn_pipebase_stop (struct nn_pipebase *self);

   /*  Call this function when new message was fully received. */
   void nn_pipebase_received (struct nn_pipebase *self);

   /*  Call this function when current outgoing message was fully sent. */
   void nn_pipebase_sent (struct nn_pipebase *self);

   /*  Retrieve value of a socket option. */
   void nn_pipebase_getopt (struct nn_pipebase *self, int level, int option,
    void *optval, size_t *optvallen);

   /*  Returns 1 is the specified socket type is a valid peer for this socket,
    or 0 otherwise. */
   int nn_pipebase_ispeer (struct nn_pipebase *self, int socktype);

   /******************************************************************************/
   /*  The transport class.                                                      */
   /******************************************************************************/

   struct nn_transport {

    /*  Name of the transport as it appears in the connection strings ("tcp",
        "ipc", "inproc" etc. */
    const char *name;

    /*  ID of the transport. */
    int id;

    /*  Following methods are guarded by a global critical section. Two of these
        function will never be invoked in parallel. The first is called when
        the library is initialised, the second one when it is terminated, i.e.
        when there are no more open sockets. Either of them can be set to NULL
        if no specific initialisation/termination is needed. */
    void (*init) (void);
    void (*term) (void);

    /*  Each of these functions creates an endpoint and returns the newly
        created endpoint in 'epbase' parameter. 'hint' is in opaque pointer
        to be passed to nn_epbase_init(). The epbase object can then be used
        to retrieve the address to bind/connect to. These functions are guarded
        by a socket-wide critical section. Two of these function will never be
        invoked in parallel on the same socket. */
    int (*bind) (void *hint, struct nn_epbase **epbase);
    int (*connect) (void *hint, struct nn_epbase **epbase);

    /*  Create an object to hold transport-specific socket options.
        Set this member to NULL in case there are no transport-specific
        socket options available. */
    struct nn_optset *(*optset) (void);

    /*  This member is used exclusively by the core. Never touch it directly
        from the transport. */
    struct nn_list_item item;
   };

   #endif
   #endif

 +/
