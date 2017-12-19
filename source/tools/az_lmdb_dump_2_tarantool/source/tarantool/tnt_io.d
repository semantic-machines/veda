module veda.bind.tarantool.tnt_io;

import veda.bind.tarantool.tnt_opt;
import veda.bind.tarantool.tnt_net;
import core.stdc.config;

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
 * \internal
 * \file tnt_io.h
 * \brief Basic network layer io
 */

tnt_error_ tnt_io_connect(tnt_stream_net *s);
void tnt_io_close(tnt_stream_net *s);

ssize_t tnt_io_flush(tnt_stream_net *s);

ssize_t tnt_io_send_raw(
                        tnt_stream_net *s,
                        const(char) *buf,
                        size_t size,
                        int all);
ssize_t tnt_io_sendv_raw(tnt_stream_net *s, iovec *iov, int count, int all);
ssize_t tnt_io_recv_raw(tnt_stream_net *s, char *buf, size_t size, int all);

ssize_t tnt_io_send(tnt_stream_net *s, const(char) *buf, size_t size);
ssize_t tnt_io_sendv(tnt_stream_net *s, iovec *iov, int count);
ssize_t tnt_io_recv(tnt_stream_net *s, char *buf, size_t size);

/* TNT_IO_H_INCLUDED */
