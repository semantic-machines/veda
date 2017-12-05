module veda.bind.tarantool.tnt_proto;

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
 * \file tnt_proto.h
 * \brief IProto protocol constants
 */

/**
 * \brief Request/response header field types (keys)
 */
enum tnt_header_key_t
{
    TNT_CODE      = 0,
    TNT_SYNC      = 1,
    TNT_SERVER_ID = 2,
    TNT_LSN       = 3,
    TNT_TIMESTAMP = 4,
    TNT_SCHEMA_ID = 5
}

/**
 * \brief Request body field types (keys)
 */
enum tnt_body_key_t
{
    TNT_SPACE        = 16,
    TNT_INDEX        = 17,
    TNT_LIMIT        = 18,
    TNT_OFFSET       = 19,
    TNT_ITERATOR     = 20,
    TNT_INDEX_BASE   = 21,
    TNT_KEY          = 32,
    TNT_TUPLE        = 33,
    TNT_FUNCTION     = 34,
    TNT_USERNAME     = 35,
    TNT_SERVER_UUID  = 36,
    TNT_CLUSTER_UUID = 37,
    TNT_VCLOCK       = 38,
    TNT_EXPRESSION   = 39,
    TNT_OPS          = 40
}

/**
 * \brief Response body field types (keys)
 */
enum tnt_response_key_t
{
    TNT_DATA  = 48,
    TNT_ERROR = 49
}

/**
 * \brief Request types
 */
enum tnt_request_t
{
    TNT_OP_SELECT    = 1,
    TNT_OP_INSERT    = 2,
    TNT_OP_REPLACE   = 3,
    TNT_OP_UPDATE    = 4,
    TNT_OP_DELETE    = 5,
    TNT_OP_CALL_16   = 6,
    TNT_OP_AUTH      = 7,
    TNT_OP_EVAL      = 8,
    TNT_OP_UPSERT    = 9,
    TNT_OP_CALL      = 10,
    TNT_OP_PING      = 64,
    TNT_OP_JOIN      = 65,
    TNT_OP_SUBSCRIBE = 66
}

/**
 * \brief Update operations
 */
enum tnt_update_op_t
{
    TNT_UOP_ADDITION  = 43,
    TNT_UOP_SUBSTRACT = 45,
    TNT_UOP_AND       = 38,
    TNT_UOP_XOR       = 94,
    TNT_UOP_OR        = 124,
    TNT_UOP_DELETE    = 35,
    TNT_UOP_INSERT    = 33,
    TNT_UOP_ASSIGN    = 61,
    TNT_UOP_SPLICE    = 58
}

/**
 * \brief Iterator types
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

/**
 * \internal
 */
enum TNT_SCRAMBLE_SIZE        = 20;
/**
 * \internal
 */
enum TNT_GREETING_SIZE        = 128;
/**
 * \internal
 */
enum TNT_VERSION_SIZE         = 64;
/**
 * \internal
 */
enum TNT_SALT_SIZE            = 44;

/**
 * \brief System spaces
 */
enum tnt_spaces_t
{
    tnt_sp_space  = 280,
    tnt_sp_index  = 288,
    tnt_sp_func   = 296,
    tnt_sp_user   = 304,
    tnt_sp_priv   = 312,
    tnt_vsp_space = 281,
    tnt_vsp_index = 289,
    tnt_vsp_func  = 297,
    tnt_vsp_user  = 305,
    tnt_vsp_priv  = 313
}

/**
 * \brief System indexes
 */
enum tnt_indexes_t
{
    tnt_vin_primary = 0,
    tnt_vin_owner   = 1,
    tnt_vin_name    = 2
}

/**
 * \brief Error code types
 */
enum tnt_errcode_t
{
    TNT_ER_UNKNOWN                           = 0,
    TNT_ER_ILLEGAL_PARAMS                    = 1,
    TNT_ER_MEMORY_ISSUE                      = 2,
    TNT_ER_TUPLE_FOUND                       = 3,
    TNT_ER_TUPLE_NOT_FOUND                   = 4,
    TNT_ER_UNSUPPORTED                       = 5,
    TNT_ER_NONMASTER                         = 6,
    TNT_ER_READONLY                          = 7,
    TNT_ER_INJECTION                         = 8,
    TNT_ER_CREATE_SPACE                      = 9,
    TNT_ER_SPACE_EXISTS                      = 10,
    TNT_ER_DROP_SPACE                        = 11,
    TNT_ER_ALTER_SPACE                       = 12,
    TNT_ER_INDEX_TYPE                        = 13,
    TNT_ER_MODIFY_INDEX                      = 14,
    TNT_ER_LAST_DROP                         = 15,
    TNT_ER_TUPLE_FORMAT_LIMIT                = 16,
    TNT_ER_DROP_PRIMARY_KEY                  = 17,
    TNT_ER_KEY_PART_TYPE                     = 18,
    TNT_ER_EXACT_MATCH                       = 19,
    TNT_ER_INVALID_MSGPACK                   = 20,
    TNT_ER_PROC_RET                          = 21,
    TNT_ER_TUPLE_NOT_ARRAY                   = 22,
    TNT_ER_FIELD_TYPE                        = 23,
    TNT_ER_FIELD_TYPE_MISMATCH               = 24,
    TNT_ER_SPLICE                            = 25,
    TNT_ER_ARG_TYPE                          = 26,
    TNT_ER_TUPLE_IS_TOO_LONG                 = 27,
    TNT_ER_UNKNOWN_UPDATE_OP                 = 28,
    TNT_ER_UPDATE_FIELD                      = 29,
    TNT_ER_FIBER_STACK                       = 30,
    TNT_ER_KEY_PART_COUNT                    = 31,
    TNT_ER_PROC_LUA                          = 32,
    TNT_ER_NO_SUCH_PROC                      = 33,
    TNT_ER_NO_SUCH_TRIGGER                   = 34,
    TNT_ER_NO_SUCH_INDEX                     = 35,
    TNT_ER_NO_SUCH_SPACE                     = 36,
    TNT_ER_NO_SUCH_FIELD                     = 37,
    TNT_ER_SPACE_FIELD_COUNT                 = 38,
    TNT_ER_INDEX_FIELD_COUNT                 = 39,
    TNT_ER_WAL_IO                            = 40,
    TNT_ER_MORE_THAN_ONE_TUPLE               = 41,
    TNT_ER_ACCESS_DENIED                     = 42,
    TNT_ER_CREATE_USER                       = 43,
    TNT_ER_DROP_USER                         = 44,
    TNT_ER_NO_SUCH_USER                      = 45,
    TNT_ER_USER_EXISTS                       = 46,
    TNT_ER_PASSWORD_MISMATCH                 = 47,
    TNT_ER_UNKNOWN_REQUEST_TYPE              = 48,
    TNT_ER_UNKNOWN_SCHEMA_OBJECT             = 49,
    TNT_ER_CREATE_FUNCTION                   = 50,
    TNT_ER_NO_SUCH_FUNCTION                  = 51,
    TNT_ER_FUNCTION_EXISTS                   = 52,
    TNT_ER_FUNCTION_ACCESS_DENIED            = 53,
    TNT_ER_FUNCTION_MAX                      = 54,
    TNT_ER_SPACE_ACCESS_DENIED               = 55,
    TNT_ER_USER_MAX                          = 56,
    TNT_ER_NO_SUCH_ENGINE                    = 57,
    TNT_ER_RELOAD_CFG                        = 58,
    TNT_ER_CFG                               = 59,
    TNT_ER_SOPHIA                            = 60,
    TNT_ER_LOCAL_SERVER_IS_NOT_ACTIVE        = 61,
    TNT_ER_UNKNOWN_SERVER                    = 62,
    TNT_ER_CLUSTER_ID_MISMATCH               = 63,
    TNT_ER_INVALID_UUID                      = 64,
    TNT_ER_CLUSTER_ID_IS_RO                  = 65,
    TNT_ER_RESERVED66                        = 66,
    TNT_ER_SERVER_ID_IS_RESERVED             = 67,
    TNT_ER_INVALID_ORDER                     = 68,
    TNT_ER_MISSING_REQUEST_FIELD             = 69,
    TNT_ER_IDENTIFIER                        = 70,
    TNT_ER_DROP_FUNCTION                     = 71,
    TNT_ER_ITERATOR_TYPE                     = 72,
    TNT_ER_REPLICA_MAX                       = 73,
    TNT_ER_INVALID_XLOG                      = 74,
    TNT_ER_INVALID_XLOG_NAME                 = 75,
    TNT_ER_INVALID_XLOG_ORDER                = 76,
    TNT_ER_NO_CONNECTION                     = 77,
    TNT_ER_TIMEOUT                           = 78,
    TNT_ER_ACTIVE_TRANSACTION                = 79,
    TNT_ER_NO_ACTIVE_TRANSACTION             = 80,
    TNT_ER_CROSS_ENGINE_TRANSACTION          = 81,
    TNT_ER_NO_SUCH_ROLE                      = 82,
    TNT_ER_ROLE_EXISTS                       = 83,
    TNT_ER_CREATE_ROLE                       = 84,
    TNT_ER_INDEX_EXISTS                      = 85,
    TNT_ER_TUPLE_REF_OVERFLOW                = 86,
    TNT_ER_ROLE_LOOP                         = 87,
    TNT_ER_GRANT                             = 88,
    TNT_ER_PRIV_GRANTED                      = 89,
    TNT_ER_ROLE_GRANTED                      = 90,
    TNT_ER_PRIV_NOT_GRANTED                  = 91,
    TNT_ER_ROLE_NOT_GRANTED                  = 92,
    TNT_ER_MISSING_SNAPSHOT                  = 93,
    TNT_ER_CANT_UPDATE_PRIMARY_KEY           = 94,
    TNT_ER_UPDATE_INTEGER_OVERFLOW           = 95,
    TNT_ER_GUEST_USER_PASSWORD               = 96,
    TNT_ER_TRANSACTION_CONFLICT              = 97,
    TNT_ER_UNSUPPORTED_ROLE_PRIV             = 98,
    TNT_ER_LOAD_FUNCTION                     = 99,
    TNT_ER_FUNCTION_LANGUAGE                 = 100,
    TNT_ER_RTREE_RECT                        = 101,
    TNT_ER_PROC_C                            = 102,
    TNT_ER_UNKNOWN_RTREE_INDEX_DISTANCE_TYPE = 103,
    TNT_ER_PROTOCOL                          = 104,
    TNT_ER_UPSERT_UNIQUE_SECONDARY_KEY       = 105,
    TNT_ER_WRONG_INDEX_RECORD                = 106,
    TNT_ER_WRONG_INDEX_PARTS                 = 107,
    TNT_ER_WRONG_INDEX_OPTIONS               = 108,
    TNT_ER_WRONG_SCHEMA_VERSION              = 109,
    TNT_ER_SLAB_ALLOC_MAX                    = 110
}

/* TNT_PROTO_H_INCLUDED */
