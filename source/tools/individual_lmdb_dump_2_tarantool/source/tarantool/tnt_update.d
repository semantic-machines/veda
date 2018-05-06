module veda.bind.tarantool.tnt_update;

import veda.bind.tarantool.tnt_stream;
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
 * \file tnt_update.h
 * \brief Update operation
 */

/**
 * \brief Generate and write update operation with predefined
 *
 * \param s     stream pointer
 * \param space space no
 * \param index index no
 * \param key   key to update
 * \param ops   ops to update (tnt_object)
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update(
                   tnt_stream *s,
                   uint space,
                   uint index,
                   tnt_stream *key,
                   tnt_stream *ops);

/**
 * \brief Add bit operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param op      operation ('&', '|', '^')
 * \param value   value for update op
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_bit(tnt_stream *ops, uint fieldno, char op, ulong value);

/**
 * \brief Add int arithmetic operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param op      operation ('+', '-')
 * \param value   value for update op
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_arith_int(
                             tnt_stream *ops,
                             uint fieldno,
                             char op,
                             long value);

/**
 * \brief Add float arithmetic operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param op      operation ('+', '-')
 * \param value   value for update op
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_arith_float(
                               tnt_stream *ops,
                               uint fieldno,
                               char op,
                               float value);

/**
 * \brief Add double arithmetic operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param op      operation ('+', '-')
 * \param value   value for update op
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_arith_double(
                                tnt_stream *ops,
                                uint fieldno,
                                char op,
                                double value);

/**
 * \brief Add delete operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param fieldco field count
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_delete(tnt_stream *ops, uint fieldno, uint fieldco);

/**
 * \brief Add insert before operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param val     value to insert (tnt_object)
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_insert(tnt_stream *ops, uint fieldno, tnt_stream *val);

/**
 * \brief Add assign operation for update to tnt_object
 *
 * \param ops     operation container
 * \param fieldno field number
 * \param val     value to assign (tnt_object)
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_assign(tnt_stream *ops, uint fieldno, tnt_stream *val);

/**
 * \brief Add splice operation for update to tnt_object
 *
 * \param ops         operation container
 * \param fieldno     field number
 * \param position    cut from
 * \param offset      number of bytes to cut
 * \param buffer      buffer to insert instead
 * \param buffer_len  buffer length
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_update_splice(
                          tnt_stream *ops,
                          uint fieldno,
                          uint position,
                          uint offset,
                          const(char) *buffer,
                          size_t buffer_len);

/**
 * \brief shortcut for tnt_object() with type == TNT_SBO_SPARSE
 */
tnt_stream *tnt_update_container(tnt_stream *ops);

/**
 * \brief shortcut for tnt_object_container_close()
 */
tnt_stream *tnt_update_container(tnt_stream *ops);

int tnt_update_container_close(tnt_stream *ops);

int tnt_update_container_reset(tnt_stream *ops);

/**
 * \brief Generate and write upsert operation with predefined
 *
 * \param s     stream pointer
 * \param space space no
 * \param tuple (tnt_object instance) msgpack array with tuple to insert to
 * \param ops   ops to update (tnt_object)
 *
 * \returns count of bytes written
 * \retval  -1 oom
 * \sa tnt_update_cointainer
 * \sa tnt_update_cointainer_close
 */
ssize_t tnt_upsert(
                   tnt_stream *s,
                   uint space,
                   tnt_stream *tuple,
                   tnt_stream *ops);

/* TNT_UPDATE_H_INCLUDED */
