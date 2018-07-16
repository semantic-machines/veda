module veda.bind.tarantool.tnt_stream;

import core.stdc.config;
import veda.bind.tarantool.tnt_request;
import veda.bind.tarantool.tnt_reply;

extern (C) :

alias c_ulong size_t;
alias c_long ssize_t;
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
 * \file tnt_stream.h
 * \brief Basic stream object
 */

/**
 * \brief Basic stream object
 * all function pointers are NULL, if operation is not supported
 */
struct iovec;
struct tnt_stream
{
    int alloc;                                                                  /*!< Allocation mark */
    ssize_t function(tnt_stream *s, const(char) *buf, size_t size) write;       /*!< write to buffer function */
    ssize_t function(tnt_stream *s, iovec *iov, int count) writev;              /*!< writev function */
    ssize_t function(tnt_stream *s, tnt_request *r, ulong *sync) write_request; /*!< write request function */

    ssize_t function(tnt_stream *s, char *buf, size_t size) read;               /*!< read from buffer function */
    int function(tnt_stream *s, tnt_reply_ *r) read_reply;                      /*!< read reply from buffer */

    void function(tnt_stream *s) free;                                          /*!< free custom buffer types (destructor) */

    void  *data;                                                                /*!< subclass data */
    uint  wrcnt;                                                                /*!< count of write operations */
    ulong reqid;                                                                /*!< request id of current operation */
}

/**
 * \brief Base function for allocating stream. For internal use only.
 */
tnt_stream *tnt_stream_init(tnt_stream *s);
/**
 * \brief Base function for freeing stream. For internal use only.
 */
void tnt_stream_free(tnt_stream *s);

/**
 * \brief set reqid number. It's incremented at every request compilation.
 * default is 0
 */
uint tnt_stream_reqid(tnt_stream *s, uint reqid);

/* TNT_STREAM_H_INCLUDED */
