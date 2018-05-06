module veda.bind.tarantool.tnt_call;

import core.stdc.config;
import veda.bind.tarantool.tnt_stream;

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
 * \file tnt_call.h
 * \brief Call request
 */

/**
 * \brief Construct call request and write it into stream
 *
 * \param s    stream object to write request to
 * \param proc procedure to call
 * \param plen procedure length
 * \param args tnt_object instance with messagepack array with args to call
 *             procedure with
 *
 * \retval number of bytes written to stream
 */


ssize_t tnt_call(
                 tnt_stream *s,
                 const(char) *proc,
                 size_t plen,
                 tnt_stream *args);

/**
 * \brief Construct call request and write it into stream
 *        Version for Tarantool 1.6
 *
 * \param s    stream object to write request to
 * \param proc procedure to call
 * \param plen procedure length
 * \param args tnt_object instance with messagepack array with args to call
 *             procedure with
 *
 * \retval number of bytes written to stream
 */

ssize_t tnt_call_16(
                    tnt_stream *s,
                    const(char) *proc,
                    size_t proc_len,
                    tnt_stream *args);

/**
 * \brief Construct eval request and write it into stream
 *
 * \param s    stream object to write request to
 * \param expr expression to evaluate
 * \param elen expression length
 * \param args tnt_object instance with messagepack array with args to eval
 *             expression with
 *
 * \retval number of bytes written to stream
 */

ssize_t tnt_eval(
                 tnt_stream *s,
                 const(char) *expr,
                 size_t elen,
                 tnt_stream *args);

/* TNT_CALL_H_INCLUDED */
