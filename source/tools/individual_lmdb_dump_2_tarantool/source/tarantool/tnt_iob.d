module veda.bind.tarantool.tnt_iob;

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
 * \file tnt_iob.h
 * \brief Basic network layer static sized buffer
 */

alias c_long function(void *ptr, const(char) *buf, size_t size) tnt_iob_tx_t;
struct iovec;
alias c_long function(void *ptr, iovec *iov, int count) tnt_iob_txv_t;

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

int tnt_iob_init(
                 tnt_iob *iob,
                 size_t size,
                 tnt_iob_tx_t tx,
                 tnt_iob_txv_t txv,
                 void *ptr);

void tnt_iob_clear(tnt_iob *iob);

void tnt_iob_free(tnt_iob *iob);

/* TNT_IOB_H_INCLUDED */
