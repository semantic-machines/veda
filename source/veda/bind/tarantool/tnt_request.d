module veda.bind.tarantool.tnt_request;

import veda.bind.tarantool.tnt_iter;
import veda.bind.tarantool.tnt_stream;
import veda.bind.tarantool.tnt_proto;
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
 * \file tnt_request.h
 * \brief Request creation using connection schema
 */

struct tnt_request
{
    /*!< Request sync id. Generated when encoded */
    /*!< Request type */
    struct _Anonymous_0
    {
        ulong sync; /*!< fields for header */
        /*!< Space number */
        /*!< Index number */
        /*!< Offset for select */
        /*!< Limit for select */
        /*!< Iterator for select */
        /* Search key, proc name or eval expression */
        /*!< Pointer for
         * key for select/update/delete,
         * procedure  for call,
         * expression for eval,
         * operations for upsert
         */

        enum tnt_request_t
        {
            /*!< Pointer for key object
             * if allocated inside requests
             * functions
             */
            TNT_OP_SELECT    = 1,
            TNT_OP_INSERT    = 2,
            TNT_OP_REPLACE   = 3,
            TNT_OP_UPDATE    = 4,
            TNT_OP_DELETE    = 5,
            /*!< Pointer for
             * tuple for insert/replace,
             * ops for update
             * default tuple for upsert,
             * args for eval/call
             */
            TNT_OP_CALL_16   = 6,
            TNT_OP_AUTH      = 7,
            TNT_OP_EVAL      = 8,
            TNT_OP_UPSERT    = 9,
            TNT_OP_CALL      = 10,
            TNT_OP_PING      = 64,
            TNT_OP_JOIN      = 65,
            TNT_OP_SUBSCRIBE = 66
        }

        tnt_request_t type;
    }

    _Anonymous_0 hdr;
    uint         space_id;
    uint         index_id;
    uint         offset;
    uint         limit;
    /*!< Pointer for tuple object
     * if allocated inside requests
     * functions
     */
    /*!< field offset for UPDATE */
    /*!< allocation mark */

    /**
     * \brief Allocate and initialize request object
     *
     * if request pointer is NULL, then new request will be created
     *
     * \param req    pointer to request
     * \param stream pointer to stream for schema (may be NULL)
     *
     * \returns pointer to request object
     * \retval  NULL memory allocation failure
     */

    enum tnt_iterator_t
    {
        TNT_ITER_EQ               = 0,
        TNT_ITER_REQ              = 1,
        TNT_ITER_ALL              = 2,
        TNT_ITER_LT               = 3,
        TNT_ITER_LE               = 4,
        TNT_ITER_GE               = 5,
        TNT_ITER_GT               = 6,
        TNT_ITER_BITS_ALL_SET     = 7,
        TNT_ITER_BITS_ANY_SET     = 8,
        TNT_ITER_BITS_ALL_NOT_SET = 9,
        TNT_ITER_OVERLAP          = 10,
        TNT_ITER_NEIGHBOR         = 11
    }

    tnt_iterator_t iterator;
    const(char)*key;
    const(char)*key_end;
    struct tnt_stream;
    tnt_stream *key_object;
    const(char)*tuple;
    const(char)*tuple_end;
    tnt_stream *tuple_object;
    int        index_base;
    int        alloc;
}

tnt_request *tnt_request_init(tnt_request *req);

/**
 * \brief Free request object
 *
 * \param req request object
 */
void tnt_request_free(tnt_request *req);

/**
 * \brief Set request space from number
 *
 * \param req   request object
 * \param space space number
 *
 * \retval 0 ok
 * \sa tnt_request_set_space
 */
int tnt_request_set_space(tnt_request *req, uint space);

/**
 * \brief Set request index from number
 *
 * \param req   request object
 * \param index index number
 *
 * \retval 0  ok
 * \sa tnt_request_set_index
 */
int tnt_request_set_index(tnt_request *req, uint index);

/**
 * \brief Set offset for select
 *
 * \param req    request pointer
 * \param offset offset to set
 *
 * \retval 0 ok
 */
int tnt_request_set_offset(tnt_request *req, uint offset);

/**
 * \brief Set limit for select
 *
 * \param req   request pointer
 * \param limit limit to set
 *
 * \retval 0 ok
 */
int tnt_request_set_limit(tnt_request *req, uint limit);

/**
 * \brief Set iterator for select
 *
 * \param req  request pointer
 * \param iter iter to set
 *
 * \retval 0 ok
 */
int tnt_request_set_iterator(tnt_request *req, tnt_iterator_t iter);

/**
 * \brief Set index base for update/upsert operation
 *
 * \param req        request pointer
 * \param index_base field offset to set
 *
 * \retval 0 ok
 */
int tnt_request_set_index_base(tnt_request *req, uint index_base);

/**
 * \brief Set key from predefined object
 *
 * \param req request pointer
 * \param s   tnt_object pointer
 *
 * \retval 0 ok
 */
int tnt_request_set_key(tnt_request *req, tnt_stream *s);

/**
 * \brief Set key from print-like function
 *
 * \param req request pointer
 * \param fmt format string
 * \param ... arguments for format string
 *
 * \retval 0  ok
 * \retval -1 oom/format error
 * \sa tnt_object_format
 */
int tnt_request_set_key_format(tnt_request *req, const(char) *fmt, ...);

/**
 * \brief Set function from string
 *
 * \param req  request pointer
 * \param func function string
 * \param flen function string length
 *
 * \retval 0 ok
 */
int tnt_request_set_func(tnt_request *req, const(char) *func, uint flen);

/**
 * \brief Set function from NULL-terminated string
 *
 * \param req  request pointer
 * \param func function string
 *
 * \retval 0 ok
 */
int tnt_request_set_funcz(tnt_request *req, const(char) *func);

/**
 * \brief Set expression from string
 *
 * \param req  request pointer
 * \param expr expression string
 * \param elen expression string length
 *
 * \retval 0  ok
 * \retval -1 error
 */
int tnt_request_set_expr(tnt_request *req, const(char) *expr, uint elen);

/**
 * \brief Set expression from NULL-terminated string
 *
 * \param req  request pointer
 * \param expr expression string
 *
 * \retval 0  ok
 * \retval -1 error
 */
int tnt_request_set_exprz(tnt_request *req, const(char) *expr);

/**
 * \brief Set tuple from predefined object
 *
 * \param req request pointer
 * \param s   tnt_object pointer
 *
 * \retval 0 ok
 */
int tnt_request_set_tuple(tnt_request *req, tnt_stream *s);

/**
 * \brief Set tuple from print-like function
 *
 * \param req request pointer
 * \param fmt format string
 * \param ... arguments for format string
 *
 * \retval 0  ok
 * \retval -1 oom/format error
 * \sa tnt_object_format
 */
int tnt_request_set_tuple_format(tnt_request *req, const(char) *fmt, ...);

/**
 * \brief Set operations from predefined object
 *
 * \param req request pointer
 * \param s   tnt_object pointer
 *
 * \retval 0 ok
 */
int tnt_request_set_ops(tnt_request *req, tnt_stream *s);

/**
 * \brief Encode request to stream object
 *
 * \param s   stream pointer
 * \param req request pointer
 *
 * \retval >0 ok, sync is returned
 * \retval -1 out of memory
 */
long tnt_request_compile(tnt_stream *s, tnt_request *req);

/**
 * \brief Encode request to stream object.
 *
 * \param[in]  s    stream pointer
 * \param[in]  req  request pointer
 * \param[out] sync pointer to compiled request
 *
 * \retval 0  ok
 * \retval -1 out of memory
 */
int tnt_request_writeout(tnt_stream *s, tnt_request *req, ulong *sync);

/**
 * \brief create select request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_select(tnt_request *req);

/**
 * \brief create insert request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_insert(tnt_request *req);

/**
 * \brief create replace request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_replace(tnt_request *req);

/**
 * \brief create update request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_update(tnt_request *req);

/**
 * \brief create delete request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_delete(tnt_request *req);

/**
 * \brief create call request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_call(tnt_request *req);

/**
 * \brief create call request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_call_16(tnt_request *req);

/**
 * \brief create auth request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_auth(tnt_request *req);

/**
 * \brief create eval request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_eval(tnt_request *req);

/**
 * \brief create upsert request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_upsert(tnt_request *req);

/**
 * \brief create ping request object
 * \sa tnt_request_init
 */
tnt_request *tnt_request_ping(tnt_request *req);

/* TNT_REQUEST_H_INCLUDED */
