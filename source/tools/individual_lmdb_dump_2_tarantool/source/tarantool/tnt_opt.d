module veda.bind.tarantool.tnt_opt;

import core.stdc.config;
import core.stdc.stdarg;
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
 * \file tnt_opt.h
 * \brief Networking layer options
 */

struct tnt_iob;

/**
 * \brief Callback type for read (instead of reading from socket)
 *
 * \param b   context for read operation
 * \param buf buf to read to
 * \param len size to read
 *
 * \returns size that was read
 * \retval  -1 error, errno must be set
 */
alias c_long function(tnt_iob *b, void *buf, size_t len) recv_cb_t;

/**
 * \brief Callback type for write (instead of writing into socket)
 *
 * \param b   context for write operation
 * \param buf buf to write
 * \param len size to write
 *
 * \returns size that was written
 * \retval  -1 error, errno must be set
 */
alias c_long function(tnt_iob *b, void *buf, size_t len) send_cb_t;

/**
 * \brief Callback type for write with iovec (instead of writing into socket)
 *
 * \param b   context for write operation
 * \param buf iovec to write
 * \param len iovec len
 *
 * \returns size that was written
 * \retval  -1 error, errno must be set
 */
struct iovec;
alias c_long function(tnt_iob *b, const(iovec) *iov, int iov_count) sendv_cb_t;

/**
 * \brief Options for connection
 */
enum tnt_opt_type
{
    TNT_OPT_URI           = 0, /*!< Options for setting URI */
    TNT_OPT_TMOUT_CONNECT = 1, /*!< Option for setting timeout on connect */
    TNT_OPT_TMOUT_RECV    = 2, /*!< Option for setting timeout on recv */
    TNT_OPT_TMOUT_SEND    = 3, /*!< Option for setting timeout in send */
    TNT_OPT_SEND_CB       = 4, /*!< callback, that's executed on send
                                * \sa send_cb_t
                                */
    TNT_OPT_SEND_CBV      = 5, /*!< callback, that's executed on send with iovector
                                * \sa sendv_cb_t
                                */
    TNT_OPT_SEND_CB_ARG   = 6, /*!< callback context for send */
    TNT_OPT_SEND_BUF      = 7, /*!< Option for setting send buffer size */
    TNT_OPT_RECV_CB       = 8, /*!< callback, that's executed on recv */
    TNT_OPT_RECV_CB_ARG   = 9, /*!< callback context for recv
                                * \sa recv_cb_t
                                */
    TNT_OPT_RECV_BUF      = 10 /*!< Option for setting recv buffer size */
}

/**
 * \internal
 * \brief structure, that is used for options
 */
struct tnt_opt
{
    const(char)*uristr;
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

/**
 * \internal
 */
int tnt_opt_init(tnt_opt *opt);

/**
 * \internal
 */
void tnt_opt_free(tnt_opt *opt);

/**
 * \internal
 */
int tnt_opt_set(tnt_opt *opt, tnt_opt_type name, va_list args);

/* TNT_OPT_H_INCLUDED */
