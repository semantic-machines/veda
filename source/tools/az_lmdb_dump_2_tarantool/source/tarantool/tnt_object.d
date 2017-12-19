module veda.bind.tarantool.tnt_object;

import veda.bind.tarantool.tnt_stream;
import core.stdc.config;
import core.stdc.stdarg;

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
 * \file tnt_object.h
 * \brief Object for manipulating msgpack objects
 */

/**
 * \brief for internal use
 */
struct tnt_sbo_stack
{
    size_t offset;
    uint   size;
    byte   type;
}

/**
 * \brief type of packing msgpack array/map
 *
 * - TNT_SBO_SIMPLE - without packing, demanding size to be specified
 * - TNT_SBO_SPARSE - 5 bytes always allocated for map/array, size is ignored
 * - TNT_SBO_PACKED - 1 byte is alloced for map/array, if needed more, then
 *                    everything is moved to n bytes, when called
 *                    "tnt_object_container_close"
 */
enum tnt_sbo_type
{
    TNT_SBO_SIMPLE = 0,
    TNT_SBO_SPARSE = 1,
    TNT_SBO_PACKED = 2
}

struct tnt_sbuf_object
{
    tnt_sbo_stack *stack;
    ubyte         stack_size;
    ubyte         stack_alloc;
    tnt_sbo_type  type;
}

extern (D) auto TNT_OBJ_CAST(T) (auto ref T SB)
{
    return cast(tnt_sbuf_object *)SB.subdata;
}

extern (D) auto TNT_SOBJ_CAST(T) (auto ref T S)
{
    return TNT_OBJ_CAST(TNT_SBUF_CAST(S));
}

/**
 * \brief Set type of packing for objects
 *
 * Type must be set before first value was written
 *
 * \param s    tnt_object instance
 * \param type type of packing
 *
 * \returns status of operation
 * \retval  -1 (something was written before
 * \retval   0 success
 */


int tnt_object_type(tnt_stream *s, tnt_sbo_type type);

/**
 * \brief create and initialize tnt_object
 *
 * tnt_object is used to create msgpack values: keys/tuples/args for
 * passing them into tnt_request or tnt_<operation>
 * if stream object is NULL, then new stream object will be created
 *
 * \param s object pointer
 *
 * \returns object pointer
 * \retval NULL error
 */

tnt_stream *tnt_object(tnt_stream *s);

/**
 * \brief Add nil to a stream object
 */
ssize_t tnt_object_add_nil(tnt_stream *s);

/**
 * \brief Add integer to a stream object
 */
ssize_t tnt_object_add_int(tnt_stream *s, long value);

/**
 * \brief Add unsigned integer to a stream object
 */
ssize_t tnt_object_add_uint(tnt_stream *s, ulong value);

/**
 * \brief Add string to a stream object
 */
ssize_t tnt_object_add_str(tnt_stream *s, const(char) *str, uint len);

/**
 * \brief Add null terminated string to a stream object
 */
ssize_t tnt_object_add_strz(tnt_stream *s, const(char) *strz);

/**
 * \brief Add binary object to a stream object
 */
ssize_t tnt_object_add_bin(tnt_stream *s, const(void) *bin, uint len);

/**
 * \brief Add boolean to a stream object
 */
ssize_t tnt_object_add_bool(tnt_stream *s, char value);

/**
 * \brief Add floating value to a stream object
 */
ssize_t tnt_object_add_float(tnt_stream *s, float value);

/**
 * \brief Add double precision floating value to a stream object
 */
ssize_t tnt_object_add_double(tnt_stream *s, double value);

/**
 * \brief Append array header to stream object
 * \sa tnt_sbo_type
 */
ssize_t tnt_object_add_array(tnt_stream *s, uint size);

/**
 * \brief Append map header to stream object
 * \sa tnt_sbo_type
 */
ssize_t tnt_object_add_map(tnt_stream *s, uint size);

/**
 * \brief Close array/map in case TNT_SBO_PACKED/TNT_SBO_SPARSE were used
 * \sa tnt_sbo_type
 */
ssize_t tnt_object_container_close(tnt_stream *s);

/**
 * \brief create immutable tnt_object from given buffer
 */
tnt_stream *tnt_object_as(tnt_stream *s, char *buf, size_t buf_len);

/**
 * \brief verify that object is valid msgpack structure
 * \param s object pointer
 * \param type -1 on check without validating type, otherwise `enum mp_type`
 */
int tnt_object_verify(tnt_stream *s, byte type);

/**
 * \brief reset tnt_object to basic state
 * this function doesn't deallocate memory, but instead it simply sets all
 * pointers to beginning
 */
int tnt_object_reset(tnt_stream *s);

/**
 * \brief create tnt_object from format string/values (va_list variation)
 *
 * \code{.c}
 * \*to get a msgpack array of two items: number 42 and map (0->"false, 2->"true")*\
 * tnt_object_format(s, "[%d {%d%s%d%s}]", 42, 0, "false", 1, "true");
 * \endcode
 *
 * \param s   tnt_object instance
 * \param fmt zero-end string, containing structure of resulting
 * msgpack and types of next arguments.
 * Format can contain '[' and ']' pairs, defining arrays,
 * '{' and '}' pairs, defining maps, and format specifiers, described below:
 * %d, %i - int
 * %u - unsigned int
 * %ld, %li - long
 * %lu - unsigned long
 * %lld, %lli - long long
 * %llu - unsigned long long
 * %hd, %hi - short
 * %hu - unsigned short
 * %hhd, %hhi - char (as number)
 * %hhu - unsigned char (as number)
 * %f - float
 * %lf - double
 * %b - bool
 * %s - zero-end string
 * %.*s - string with specified length
 * %% is ignored
 * %'smth else' assert and undefined behaviour
 * NIL - a nil value
 * all other symbols are ignored.
 *
 * \sa tnt_object_vformat
 * \sa tnt_object_format
 */
ssize_t tnt_object_format(tnt_stream *s, const(char) *fmt, ...);

/**
 * \brief create tnt_object from format string/values
 * \sa tnt_object_vformat
 * \sa tnt_object_format
 */
ssize_t tnt_object_vformat(tnt_stream *s, const(char) *fmt, va_list vl);

/* TNT_OBJECT_H_INCLUDED */
