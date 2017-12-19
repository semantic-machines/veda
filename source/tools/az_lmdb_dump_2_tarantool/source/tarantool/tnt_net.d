module veda.bind.tarantool.tnt_net;

import veda.bind.tarantool.tnt_iob;
import veda.bind.tarantool.tnt_opt;
import veda.bind.tarantool.tnt_stream;

import core.stdc.config;
import core.stdc.time;

extern (C) :

alias c_ulong size_t;
alias c_long ssize_t;
alias c_long __suseconds_t;

/*
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 * 1. Redistributions of source code must retain the above
 *    copyright notice, this list of conditions and the
 *    following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials
 *    provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY <COPYRIGHT HOLDER> ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * <COPYRIGHT HOLDER> OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/**
 * \file tnt_net.h
 * \brief Basic tarantool client library header for network stream layer
 */

/**
 * \brief Internal error codes
 */
enum tnt_error_
{
    TNT_EOK      = 0, /*!< Everything is OK */
    TNT_EFAIL    = 1, /*!< Fail */
    TNT_EMEMORY  = 2, /*!< Memory allocation failed */
    TNT_ESYSTEM  = 3, /*!< System error */
    TNT_EBIG     = 4, /*!< Buffer is too big */
    TNT_ESIZE    = 5, /*!< Bad buffer size */
    TNT_ERESOLVE = 6, /*!< gethostbyname(2) failed */
    TNT_ETMOUT   = 7, /*!< Operation timeout */
    TNT_EBADVAL  = 8, /*!< Bad argument (value) */
    TNT_ELOGIN   = 9, /*!< Failed to login */
    TNT_LAST     = 10 /*!< Not an error */
}

/**
 * \brief Network stream structure
 */
struct tnt_stream_net
{
    /*!< Options for connection */
    /*!< Connection status. 1 - true, 0 - false */
    /*!< fd of connection */
    /*!< Send buffer */
    /*!< Recv buffer */
    /*!< If retval == -1, then error is set. */
    /*!< If TNT_ESYSTEM then errno_ is set */
    /*!< Pointer to greeting, if connected */
    /*!< Collation for space/index string<->number */
    /*!< 1 if iob/schema were allocated */

    /*!
     * \internal
     * \brief Cast tnt_stream to tnt_net
     */

    /**
     * \brief Create tnt_net stream instance
     *
     * \param s stream pointer, maybe NULL
     *
     * If stream pointer is NULL, then new stream will be created.
     *
     * \returns stream pointer
     * \retval NULL oom
     *
     * \code{.c}
     * struct tnt_stream *tnt = tnt_net(NULL);
     * assert(tnt);
     * assert(tnt_set(s, TNT_OPT_URI, "login:passw@localhost:3302") != -1);
     * assert(tnt_connect(s) != -1);
     * ...
     * tnt_close(s);
     * \endcode
     */
    struct tnt_opt
    {
        const(char)*uristr;

        /**
         * \brief Set options for connection
         *
         * \param s   stream pointer
         * \param opt option to set
         * \param ... option value
         *
         * \returns status
         * \retval -1 error
         * \retval  0 ok
         * \sa enum tnt_opt_type
         *
         * \code{.c}
         * assert(tnt_set(s, TNT_OPT_SEND_BUF, 16*1024) != -1);
         * assert(tnt_set(s, TNT_OPT_RECV_BUF, 16*1024) != -1);
         * assert(tnt_set(s, TNT_OPT_URI, "login:passw@localhost:3302") != -1);
         * \endcode
         *
         * \note
         * URI format:
         * * "[login:password@]host:port" for tcp sockets
         * * "[login:password@]/tmp/socket_path.sock" for unix sockets
         * \sa enum tnt_opt_type
         */
        struct uri;
        //uri* uri;

        struct timeval
        {
            time_t        tv_sec;
            __suseconds_t tv_usec;
        }

        timeval tmout_connect;
        timeval tmout_recv;
        timeval tmout_send;
        void    *send_cb;
        void    *send_cbv;
        void    *send_cb_arg;
        int     send_buf;
        void    *recv_cb;
        void    *recv_cb_arg;
        int     recv_buf;
    }

    tnt_opt opt;
    int     connected;
    int     fd;

    struct tnt_iob
    {
        char          *buf;
        size_t        off;
        size_t        top;
        size_t        size;
        tnt_iob_tx_t  tx;
        tnt_iob_txv_t txv;
        void          *ptr;
    }

    tnt_iob    sbuf;

    tnt_iob    rbuf;
    tnt_error_ error;
    int        errno_;
    char       *greeting;
    struct tnt_schema;
    tnt_schema *schema;
    int        inited;
}

extern (D) auto TNT_SNET_CAST(T) (auto ref T S)
{
    return cast(tnt_stream_net *)S.data;
}

tnt_stream *tnt_net(tnt_stream *s);
int tnt_set(tnt_stream *s, int opt, ...);

/*!
 * \internal
 * \brief Initialize network stream
 *
 * It must happened before connection, but after options are set.
 * 1) creation of tnt_iob's (sbuf,rbuf)
 * 2) schema creation
 *
 * \param s stream for initialization
 *
 * \returns status
 * \retval 0  ok
 * \retval -1 error (oom/einval)
 */
int tnt_init(tnt_stream *s);

/**
 * \brief Connect to tarantool with preconfigured and allocated settings
 *
 * \param s stream pointer
 *
 * \retval 0  ok
 * \retval -1 error (network/oom)
 */
int tnt_connect(tnt_stream *s);

/**
 * \brief Close connection
 * \param s stream pointer
 */
void tnt_close(tnt_stream *s);

/**
 * \brief Send written to buffer queries
 *
 * \param s tnt_stream
 *
 * \returns number of bytes written to socket
 * \retval -1 on network error
 */
ssize_t tnt_flush(tnt_stream *s);

/**
 * \brief Get tnt_net stream fd
 */
int tnt_fd(tnt_stream *s);

/**
 * \brief Error accessor for tnt_net stream
 */
tnt_error_ tnt_error(tnt_stream *s);

/**
 * \brief Format error as string
 */
char *tnt_strerror(tnt_stream *s);

/**
 * \brief Get last errno on socket
 */
int tnt_errno(tnt_stream *s);

/**
 * \brief Flush space/index schema and get it from server
 *
 * \param s stream pointer
 *
 * \returns result
 * \retval  -1 error
 * \retval  0  ok
 */
int tnt_reload_schema(tnt_stream *s);

/**
 * \brief Get space number from space name
 *
 * \returns space number
 * \retval  -1 error
 */
int tnt_get_spaceno(tnt_stream *s, const(char) *space, size_t space_len);

/**
 * \brief Get index number from index name and spaceid
 *
 * \returns index number
 * \retval  -1 error
 */
int tnt_get_indexno(
                    tnt_stream *s,
                    int spaceno,
                    const(char) *index,
                    size_t index_len);

/* TNT_NET_H_INCLUDED */
