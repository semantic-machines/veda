module veda.bind.msgpuck;

import core.stdc.config;
import core.stdc.stdio;
import core.stdc.stdarg;

extern (C) :

/*
 * Copyright (c) 2013 MsgPuck Authors
 * All rights reserved.
 *
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
 * \file msgpuck.h
 * MsgPuck
 * \brief MsgPuck is a simple and efficient MsgPack encoder/decoder
 * library in a single self-contained file.
 *
 * Usage example:
 * \code
 * // Encode
 * char buf[1024];
 * char *w = buf;
 * w = mp_encode_array(w, 4)
 * w = mp_encode_uint(w, 10);
 * w = mp_encode_str(w, "hello world", strlen("hello world"));
 * w = mp_encode_bool(w, true);
 * w = mp_encode_double(w, 3.1415);
 *
 * // Validate
 * const char *b = buf;
 * int r = mp_check(&b, w);
 * assert(!r)
 * assert(b == w);
 *
 * // Decode
 * uint32_t size;
 * uint64_t ival;
 * const char *sval;
 * uint32_t sval_len;
 * bool bval;
 * double dval;
 *
 * const char *r = buf;
 *
 * size = mp_decode_array(&r);
 * // size is 4
 *
 * ival = mp_decode_uint(&r);
 * // ival is 10;
 *
 * sval = mp_decode_str(&r, &sval_len);
 * // sval is "hello world", sval_len is strlen("hello world")
 *
 * bval = mp_decode_bool(&r);
 * // bval is true
 *
 * dval = mp_decode_double(&r);
 * // dval is 3.1415
 *
 * assert(r == w);
 * \endcode
 *
 * \note Supported compilers.
 * The implementation requires a C99+ or C++03+ compatible compiler.
 *
 * \note Inline functions.
 * The implementation is compatible with both C99 and GNU inline functions.
 * Please define MP_SOURCE 1 before \#include <msgpuck.h> in a single
 * compilation unit. This module will be used to store non-inlined versions of
 * functions and global tables.
 */

/* make С++ to be happy */

/* make С++ to be happy */

//#include <cstdbool.h>

//#include <stdarg.h>

/* defined(__cplusplus) */

/*
 * {{{ Platform-specific definitions
 */

/** \cond 0 **/

/* set the alignment to 1 for armcc compiler */

/* defined(MP_SOURCE) */

/* C99 inline */

/* defined(MP_SOURCE) */

/* GNU inline or C99 inline */

//alias mp_bswap_u16 = __builtin_bswap16;
/* !MP_GCC_VERSION(4, 8) */

//alias mp_bswap_u32 = __builtin_bswap32;
/* !MP_GCC_VERSION(4, 3) */

//alias mp_bswap_u64 = __builtin_bswap64;
/* !MP_GCC_VERSION(4, 3) */

//enum __FLOAT_WORD_ORDER__ = __BYTE_ORDER__;
/* defined(__FLOAT_WORD_ORDER__) */

/** \endcond */

/*
 * }}}
 */

/*
 * {{{ API definition
 */

/**
 * \brief MsgPack data types
 */
enum mp_type
{
    MP_NIL    = 0,
    MP_UINT   = 1,
    MP_INT    = 2,
    MP_STR    = 3,
    MP_BIN    = 4,
    MP_ARRAY  = 5,
    MP_MAP    = 6,
    MP_BOOL   = 7,
    MP_FLOAT  = 8,
    MP_DOUBLE = 9,
    MP_EXT    = 10
}

/**
 * \brief Determine MsgPack type by a first byte \a c of encoded data.
 *
 * Example usage:
 * \code
 * assert(MP_ARRAY == mp_typeof(0x90));
 * \endcode
 *
 * \param c - a first byte of encoded data
 * \return MsgPack type
 */
//MP_PROTO __attribute__((pure)) enum mp_type
mp_type mp_typeof(const char c);

/**
 * \brief Calculate exact buffer size needed to store an array header of
 * \a size elements. Maximum return value is 5. For performance reasons you
 * can preallocate buffer for maximum size without calling the function.
 * \param size - a number of elements
 * \return buffer size in bytes (max is 5)
 */
uint mp_sizeof_array(uint size);

/**
 * \brief Encode an array header of \a size elements.
 *
 * All array members must be encoded after the header.
 *
 * Example usage:
 * \code
 * // Encode
 * char buf[1024];
 * char *w = buf;
 * w = mp_encode_array(w, 2)
 * w = mp_encode_uint(w, 10);
 * w = mp_encode_uint(w, 15);
 *
 * // Decode
 * const char *r = buf;
 * uint32_t size = mp_decode_array(&r);
 * for (uint32_t i = 0; i < size; i++) {
 *     uint64_t val = mp_decode_uint(&r);
 * }
 * assert (r == w);
 * \endcode
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param size - a number of elements
 * \return \a data + \link mp_sizeof_array() mp_sizeof_array(size) \endlink
 * \sa mp_sizeof_array
 */
char *mp_encode_array(char *data, uint size);

/**
 * \brief Check that \a cur buffer has enough bytes to decode an array header
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_ARRAY
 */
ptrdiff_t mp_check_array(const(char) *cur, const(char) *end);

/**
 * \brief Decode an array header from MsgPack \a data.
 *
 * All array members must be decoded after the header.
 * \param data - the pointer to a buffer
 * \return the number of elements in an array
 * \post *data = *data + mp_sizeof_array(retval)
 * \sa \link mp_encode_array() An usage example \endlink
 */
uint mp_decode_array(const(char *) *data);

/**
 * \brief Calculate exact buffer size needed to store a map header of
 * \a size elements. Maximum return value is 5. For performance reasons you
 * can preallocate buffer for maximum size without calling the function.
 * \param size - a number of elements
 * \return buffer size in bytes (max is 5)
 */
uint mp_sizeof_map(uint size);

/**
 * \brief Encode a map header of \a size elements.
 *
 * All map key-value pairs must be encoded after the header.
 *
 * Example usage:
 * \code
 * char buf[1024];
 *
 * // Encode
 * char *w = buf;
 * w = mp_encode_map(b, 2);
 * w = mp_encode_str(b, "key1", 4);
 * w = mp_encode_str(b, "value1", 6);
 * w = mp_encode_str(b, "key2", 4);
 * w = mp_encode_str(b, "value2", 6);
 *
 * // Decode
 * const char *r = buf;
 * uint32_t size = mp_decode_map(&r);
 * for (uint32_t i = 0; i < size; i++) {
 *      // Use switch(mp_typeof(**r)) to support more types
 *     uint32_t key_len, val_len;
 *     const char *key = mp_decode_str(&r, key_len);
 *     const char *val = mp_decode_str(&r, val_len);
 * }
 * assert (r == w);
 * \endcode
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param size - a number of key/value pairs
 * \return \a data + \link mp_sizeof_map() mp_sizeof_map(size)\endlink
 * \sa mp_sizeof_map
 */
char *mp_encode_map(char *data, uint size);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a map header
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_MAP
 */
ptrdiff_t mp_check_map(const(char) *cur, const(char) *end);

/**
 * \brief Decode a map header from MsgPack \a data.
 *
 * All map key-value pairs must be decoded after the header.
 * \param data - the pointer to a buffer
 * \return the number of key/value pairs in a map
 * \post *data = *data + mp_sizeof_array(retval)
 * \sa \link mp_encode_map() An usage example \endlink
 */
uint mp_decode_map(const(char *) *data);

/**
 * \brief Calculate exact buffer size needed to store an integer \a num.
 * Maximum return value is 9. For performance reasons you can preallocate
 * buffer for maximum size without calling the function.
 * Example usage:
 * \code
 * char **data = ...;
 * char *end = *data;
 * my_buffer_ensure(mp_sizeof_uint(x), &end);
 * // my_buffer_ensure(9, &end);
 * mp_encode_uint(buffer, x);
 * \endcode
 * \param num - a number
 * \return buffer size in bytes (max is 9)
 */
uint mp_sizeof_uint(ulong num);

/**
 * \brief Calculate exact buffer size needed to store an integer \a num.
 * Maximum return value is 9. For performance reasons you can preallocate
 * buffer for maximum size without calling the function.
 * \param num - a number
 * \return buffer size in bytes (max is 9)
 * \pre \a num < 0
 */
uint mp_sizeof_int(long num);

/**
 * \brief Encode an unsigned integer \a num.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param num - a number
 * \return \a data + mp_sizeof_uint(\a num)
 * \sa \link mp_encode_array() An usage example \endlink
 * \sa mp_sizeof_uint()
 */
char *mp_encode_uint(char *data, ulong num);

/**
 * \brief Encode a signed integer \a num.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param num - a number
 * \return \a data + mp_sizeof_int(\a num)
 * \sa \link mp_encode_array() An usage example \endlink
 * \sa mp_sizeof_int()
 * \pre \a num < 0
 */
char *mp_encode_int(char *data, long num);

/**
 * \brief Check that \a cur buffer has enough bytes to decode an uint
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_UINT
 */
ptrdiff_t mp_check_uint(const(char) *cur, const(char) *end);

/**
 * \brief Check that \a cur buffer has enough bytes to decode an int
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_INT
 */
ptrdiff_t mp_check_int(const(char) *cur, const(char) *end);

/**
 * \brief Decode an unsigned integer from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return an unsigned number
 * \post *data = *data + mp_sizeof_uint(retval)
 */
ulong mp_decode_uint(const(char *) *data);

/**
 * \brief Decode a signed integer from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return an unsigned number
 * \post *data = *data + mp_sizeof_int(retval)
 */
long mp_decode_int(const(char *) *data);

/**
 * \brief Compare two packed unsigned integers.
 *
 * The function is faster than two mp_decode_uint() calls.
 * \param data_a unsigned int a
 * \param data_b unsigned int b
 * \retval < 0 when \a a < \a b
 * \retval   0 when \a a == \a b
 * \retval > 0 when \a a > \a b
 */
int mp_compare_uint(const(char) *data_a, const(char) *data_b);

/**
 * \brief Calculate exact buffer size needed to store a float \a num.
 * The return value is always 5. The function was added to provide integrity of
 * the library.
 * \param num - a float
 * \return buffer size in bytes (always 5)
 */
uint mp_sizeof_float(float num);

/**
 * \brief Calculate exact buffer size needed to store a double \a num.
 * The return value is either 5 or 9. The function was added to provide
 * integrity of the library. For performance reasons you can preallocate buffer
 * for maximum size without calling the function.
 * \param num - a double
 * \return buffer size in bytes (5 or 9)
 */
uint mp_sizeof_double(double num);

/**
 * \brief Encode a float \a num.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param num - a float
 * \return \a data + mp_sizeof_float(\a num)
 * \sa mp_sizeof_float()
 * \sa \link mp_encode_array() An usage example \endlink
 */
char *mp_encode_float(char *data, float num);

/**
 * \brief Encode a double \a num.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param num - a float
 * \return \a data + mp_sizeof_double(\a num)
 * \sa \link mp_encode_array() An usage example \endlink
 * \sa mp_sizeof_double()
 */
char *mp_encode_double(char *data, double num);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a float
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_FLOAT
 */
ptrdiff_t mp_check_float(const(char) *cur, const(char) *end);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a double
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_DOUBLE
 */
ptrdiff_t mp_check_double(const(char) *cur, const(char) *end);

/**
 * \brief Decode a float from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return a float
 * \post *data = *data + mp_sizeof_float(retval)
 */
float mp_decode_float(const(char *) *data);

/**
 * \brief Decode a double from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return a double
 * \post *data = *data + mp_sizeof_double(retval)
 */
double mp_decode_double(const(char *) *data);

/**
 * \brief Calculate exact buffer size needed to store a string header of
 * length \a num. Maximum return value is 5. For performance reasons you can
 * preallocate buffer for maximum size without calling the function.
 * \param len - a string length
 * \return size in chars (max is 5)
 */
uint mp_sizeof_strl(uint len);

/**
 * \brief Equivalent to mp_sizeof_strl(\a len) + \a len.
 * \param len - a string length
 * \return size in chars (max is 5 + \a len)
 */
uint mp_sizeof_str(uint len);

/**
 * \brief Calculate exact buffer size needed to store a binstring header of
 * length \a num. Maximum return value is 5. For performance reasons you can
 * preallocate buffer for maximum size without calling the function.
 * \param len - a string length
 * \return size in chars (max is 5)
 */
uint mp_sizeof_binl(uint len);

/**
 * \brief Equivalent to mp_sizeof_binl(\a len) + \a len.
 * \param len - a string length
 * \return size in chars (max is 5 + \a len)
 */
uint mp_sizeof_bin(uint len);

/**
 * \brief Encode a string header of length \a len.
 *
 * The function encodes MsgPack header (\em only header) for a string of
 * length \a len. You should append actual string data to the buffer manually
 * after encoding the header (exactly \a len bytes without trailing '\0').
 *
 * This approach is very useful for cases when the total length of the string
 * is known in advance, but the string data is not stored in a single
 * continuous buffer (e.g. network packets).
 *
 * It is your responsibility to ensure that \a data has enough space.
 * Usage example:
 * \code
 * char buffer[1024];
 * char *b = buffer;
 * b = mp_encode_strl(b, hdr.total_len);
 * char *s = b;
 * memcpy(b, pkt1.data, pkt1.len)
 * b += pkt1.len;
 * // get next packet
 * memcpy(b, pkt2.data, pkt2.len)
 * b += pkt2.len;
 * // get next packet
 * memcpy(b, pkt1.data, pkt3.len)
 * b += pkt3.len;
 *
 * // Check that all data was received
 * assert(hdr.total_len == (uint32_t) (b - s))
 * \endcode
 * Hint: you can dynamically reallocate the buffer during the process.
 * \param data - a buffer
 * \param len - a string length
 * \return \a data + mp_sizeof_strl(len)
 * \sa mp_sizeof_strl()
 */
char *mp_encode_strl(char *data, uint len);

/**
 * \brief Encode a string of length \a len.
 * The function is equivalent to mp_encode_strl() + memcpy.
 * \param data - a buffer
 * \param str - a pointer to string data
 * \param len - a string length
 * \return \a data + mp_sizeof_str(len) ==
 * data + mp_sizeof_strl(len) + len
 * \sa mp_encode_strl
 */
char *mp_encode_str(char *data, const(char) *str, uint len);

/**
 * \brief Encode a binstring header of length \a len.
 * See mp_encode_strl() for more details.
 * \param data - a bufer
 * \param len - a string length
 * \return data + mp_sizeof_binl(\a len)
 * \sa mp_encode_strl
 */
char *mp_encode_binl(char *data, uint len);

/**
 * \brief Encode a binstring of length \a len.
 * The function is equivalent to mp_encode_binl() + memcpy.
 * \param data - a buffer
 * \param str - a pointer to binstring data
 * \param len - a binstring length
 * \return \a data + mp_sizeof_bin(\a len) ==
 * data + mp_sizeof_binl(\a len) + \a len
 * \sa mp_encode_strl
 */
char *mp_encode_bin(char *data, const(char) *str, uint len);

/**
 * \brief Encode a sequence of values according to format string.
 * Example: mp_format(buf, sz, "[%d {%d%s%d%s}]", 42, 0, "false", 1, "true");
 * to get a msgpack array of two items: number 42 and map (0->"false, 2->"true")
 * Does not write items that don't fit to data_size argument.
 *
 * \param data - a buffer
 * \param data_size - a buffer size
 * \param format - zero-end string, containing structure of resulting
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
 * %<smth else> assert and undefined behaviour
 * NIL - a nil value
 * all other symbols are ignored.
 *
 * \return the number of requred bytes.
 * \retval > data_size means that is not enough space
 * and whole msgpack was not encoded.
 */
size_t mp_format(char *data, size_t data_size, const(char) *format, ...);

/**
 * \brief mp_format variation, taking variable argument list
 * Example:
 *  va_list args;
 *  va_start(args, fmt);
 *  mp_vformat(data, data_size, fmt, args);
 *  va_end(args);
 * \sa \link mp_format()
 */
size_t mp_vformat(
                  char *data,
                  size_t data_size,
                  const(char) *format,
                  va_list args);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a string header
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_STR
 */
ptrdiff_t mp_check_strl(const(char) *cur, const(char) *end);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a binstring header
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_BIN
 */
ptrdiff_t mp_check_binl(const(char) *cur, const(char) *end);

/**
 * \brief Decode a length of a string from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return a length of astring
 * \post *data = *data + mp_sizeof_strl(retval)
 * \sa mp_encode_strl
 */
uint mp_decode_strl(const(char *) *data);

/**
 * \brief Decode a string from MsgPack \a data
 * \param data - the pointer to a buffer
 * \param len - the pointer to save a string length
 * \return a pointer to a decoded string
 * \post *data = *data + mp_sizeof_str(*len)
 * \sa mp_encode_binl
 */
char *mp_decode_str(const(char *) *data, uint *len);

/**
 * \brief Decode a length of a binstring from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return a length of a binstring
 * \post *data = *data + mp_sizeof_binl(retval)
 * \sa mp_encode_binl
 */
uint mp_decode_binl(const(char *) *data);

/**
 * \brief Decode a binstring from MsgPack \a data
 * \param data - the pointer to a buffer
 * \param len - the pointer to save a binstring length
 * \return a pointer to a decoded binstring
 * \post *data = *data + mp_sizeof_str(*len)
 * \sa mp_encode_binl
 */
const(char) *mp_decode_bin(const(char *) *data, uint *len);

/**
 * \brief Calculate exact buffer size needed to store the nil value.
 * The return value is always 1. The function was added to provide integrity of
 * the library.
 * \return buffer size in bytes (always 1)
 */
uint mp_sizeof_nil();

/**
 * \brief Encode the nil value.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \return \a data + mp_sizeof_nil()
 * \sa \link mp_encode_array() An usage example \endlink
 * \sa mp_sizeof_nil()
 */
char *mp_encode_nil(char *data);

/**
 * \brief Check that \a cur buffer has enough bytes to decode nil
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_NIL
 */
ptrdiff_t mp_check_nil(const(char) *cur, const(char) *end);

/**
 * \brief Decode the nil value from MsgPack \a data
 * \param data - the pointer to a buffer
 * \post *data = *data + mp_sizeof_nil()
 */
void mp_decode_nil(const(char *) *data);

/**
 * \brief Calculate exact buffer size needed to store a boolean value.
 * The return value is always 1. The function was added to provide integrity of
 * the library.
 * \return buffer size in bytes (always 1)
 */
uint mp_sizeof_bool(bool val);

/**
 * \brief Encode a bool value \a val.
 * It is your responsibility to ensure that \a data has enough space.
 * \param data - a buffer
 * \param val - a bool
 * \return \a data + mp_sizeof_bool(val)
 * \sa \link mp_encode_array() An usage example \endlink
 * \sa mp_sizeof_bool()
 */
char *mp_encode_bool(char *data, bool val);

/**
 * \brief Check that \a cur buffer has enough bytes to decode a bool value
 * \param cur buffer
 * \param end end of the buffer
 * \retval 0 - buffer has enough bytes
 * \retval > 0 - the number of remaining bytes to read
 * \pre cur < end
 * \pre mp_typeof(*cur) == MP_BOOL
 */
ptrdiff_t mp_check_bool(const(char) *cur, const(char) *end);

/**
 * \brief Decode a bool value from MsgPack \a data
 * \param data - the pointer to a buffer
 * \return a decoded bool value
 * \post *data = *data + mp_sizeof_bool(retval)
 */
bool mp_decode_bool(const(char *) *data);

/**
 * \brief Skip one element in a packed \a data.
 *
 * The function is faster than mp_typeof + mp_decode_XXX() combination.
 * For arrays and maps the function also skips all members.
 * For strings and binstrings the function also skips the string data.
 *
 * Usage example:
 * \code
 * char buf[1024];
 *
 * char *w = buf;
 * // First MsgPack object
 * w = mp_encode_uint(w, 10);
 *
 * // Second MsgPack object
 * w = mp_encode_array(w, 4);
 *    w = mp_encode_array(w, 2);
 *         // Begin of an inner array
 *         w = mp_encode_str(w, "second inner 1", 14);
 *         w = mp_encode_str(w, "second inner 2", 14);
 *         // End of an inner array
 *    w = mp_encode_str(w, "second", 6);
 *    w = mp_encode_uint(w, 20);
 *    w = mp_encode_bool(w, true);
 *
 * // Third MsgPack object
 * w = mp_encode_str(w, "third", 5);
 * // EOF
 *
 * const char *r = buf;
 *
 * // First MsgPack object
 * assert(mp_typeof(**r) == MP_UINT);
 * mp_next(&r); // skip the first object
 *
 * // Second MsgPack object
 * assert(mp_typeof(**r) == MP_ARRAY);
 * mp_decode_array(&r);
 *     assert(mp_typeof(**r) == MP_ARRAY); // inner array
 *     mp_next(&r); // -->> skip the entire inner array (with all members)
 *     assert(mp_typeof(**r) == MP_STR); // second
 *     mp_next(&r);
 *     assert(mp_typeof(**r) == MP_UINT); // 20
 *     mp_next(&r);
 *     assert(mp_typeof(**r) == MP_BOOL); // true
 *     mp_next(&r);
 *
 * // Third MsgPack object
 * assert(mp_typeof(**r) == MP_STR); // third
 * mp_next(&r);
 *
 * assert(r == w); // EOF
 *
 * \endcode
 * \param data - the pointer to a buffer
 * \post *data = *data + mp_sizeof_TYPE() where TYPE is mp_typeof(**data)
 */
void mp_next(const(char *) *data);

/**
 * \brief Equivalent to mp_next() but also validates MsgPack in \a data.
 * \param data - the pointer to a buffer
 * \param end - the end of a buffer
 * \retval 0 when MsgPack in \a data is valid.
 * \retval != 0 when MsgPack in \a data is not valid.
 * \post *data = *data + mp_sizeof_TYPE() where TYPE is mp_typeof(**data)
 * \post *data is not defined if MsgPack is not valid
 * \sa mp_next()
 */
int mp_check(const(char *) *data, const(char) *end);

/*
 * }}}
 */

/*
 * {{{ Implementation
 */

/** \cond 0 */
//extern const enum mp_type mp_type_hint[];
//extern const int8_t mp_parser_hint[];

uint mp_decode_array_slowpath(ubyte c, const(char *) *data);

/** See mp_parser_hint */
enum
{
    MP_HINT          = -32,
    MP_HINT_STR_8    = -32,
    MP_HINT_STR_16   = -33,
    MP_HINT_STR_32   = -34,
    MP_HINT_ARRAY_16 = -35,
    MP_HINT_ARRAY_32 = -36,
    MP_HINT_MAP_16   = -37,
    MP_HINT_MAP_32   = -38,
    MP_HINT_EXT_8    = -39,
    MP_HINT_EXT_16   = -40,
    MP_HINT_EXT_32   = -41
}

void mp_next_slowpath(const(char *) *data, int k);

/** \endcond */

/*
 * }}}
 */

/*
 * {{{ Implementation: parser tables
 */

/** \cond 0 */

/**
 * This lookup table used by mp_sizeof() to determine enum mp_type by the first
 * byte of MsgPack element.
 */

/* {{{ MP_UINT (fixed) */
/* 0x00 */
/* 0x01 */
/* 0x02 */
/* 0x03 */
/* 0x04 */
/* 0x05 */
/* 0x06 */
/* 0x07 */
/* 0x08 */
/* 0x09 */
/* 0x0a */
/* 0x0b */
/* 0x0c */
/* 0x0d */
/* 0x0e */
/* 0x0f */
/* 0x10 */
/* 0x11 */
/* 0x12 */
/* 0x13 */
/* 0x14 */
/* 0x15 */
/* 0x16 */
/* 0x17 */
/* 0x18 */
/* 0x19 */
/* 0x1a */
/* 0x1b */
/* 0x1c */
/* 0x1d */
/* 0x1e */
/* 0x1f */
/* 0x20 */
/* 0x21 */
/* 0x22 */
/* 0x23 */
/* 0x24 */
/* 0x25 */
/* 0x26 */
/* 0x27 */
/* 0x28 */
/* 0x29 */
/* 0x2a */
/* 0x2b */
/* 0x2c */
/* 0x2d */
/* 0x2e */
/* 0x2f */
/* 0x30 */
/* 0x31 */
/* 0x32 */
/* 0x33 */
/* 0x34 */
/* 0x35 */
/* 0x36 */
/* 0x37 */
/* 0x38 */
/* 0x39 */
/* 0x3a */
/* 0x3b */
/* 0x3c */
/* 0x3d */
/* 0x3e */
/* 0x3f */
/* 0x40 */
/* 0x41 */
/* 0x42 */
/* 0x43 */
/* 0x44 */
/* 0x45 */
/* 0x46 */
/* 0x47 */
/* 0x48 */
/* 0x49 */
/* 0x4a */
/* 0x4b */
/* 0x4c */
/* 0x4d */
/* 0x4e */
/* 0x4f */
/* 0x50 */
/* 0x51 */
/* 0x52 */
/* 0x53 */
/* 0x54 */
/* 0x55 */
/* 0x56 */
/* 0x57 */
/* 0x58 */
/* 0x59 */
/* 0x5a */
/* 0x5b */
/* 0x5c */
/* 0x5d */
/* 0x5e */
/* 0x5f */
/* 0x60 */
/* 0x61 */
/* 0x62 */
/* 0x63 */
/* 0x64 */
/* 0x65 */
/* 0x66 */
/* 0x67 */
/* 0x68 */
/* 0x69 */
/* 0x6a */
/* 0x6b */
/* 0x6c */
/* 0x6d */
/* 0x6e */
/* 0x6f */
/* 0x70 */
/* 0x71 */
/* 0x72 */
/* 0x73 */
/* 0x74 */
/* 0x75 */
/* 0x76 */
/* 0x77 */
/* 0x78 */
/* 0x79 */
/* 0x7a */
/* 0x7b */
/* 0x7c */
/* 0x7d */
/* 0x7e */
/* 0x7f */
/* }}} */

/* {{{ MP_MAP (fixed) */
/* 0x80 */
/* 0x81 */
/* 0x82 */
/* 0x83 */
/* 0x84 */
/* 0x85 */
/* 0x86 */
/* 0x87 */
/* 0x88 */
/* 0x89 */
/* 0x8a */
/* 0x8b */
/* 0x8c */
/* 0x8d */
/* 0x8e */
/* 0x8f */
/* }}} */

/* {{{ MP_ARRAY (fixed) */
/* 0x90 */
/* 0x91 */
/* 0x92 */
/* 0x93 */
/* 0x94 */
/* 0x95 */
/* 0x96 */
/* 0x97 */
/* 0x98 */
/* 0x99 */
/* 0x9a */
/* 0x9b */
/* 0x9c */
/* 0x9d */
/* 0x9e */
/* 0x9f */
/* }}} */

/* {{{ MP_STR (fixed) */
/* 0xa0 */
/* 0xa1 */
/* 0xa2 */
/* 0xa3 */
/* 0xa4 */
/* 0xa5 */
/* 0xa6 */
/* 0xa7 */
/* 0xa8 */
/* 0xa9 */
/* 0xaa */
/* 0xab */
/* 0xac */
/* 0xad */
/* 0xae */
/* 0xaf */
/* 0xb0 */
/* 0xb1 */
/* 0xb2 */
/* 0xb3 */
/* 0xb4 */
/* 0xb5 */
/* 0xb6 */
/* 0xb7 */
/* 0xb8 */
/* 0xb9 */
/* 0xba */
/* 0xbb */
/* 0xbc */
/* 0xbd */
/* 0xbe */
/* 0xbf */
/* }}} */

/* {{{ MP_NIL, MP_BOOL */
/* 0xc0 */
/* 0xc1 */ /* never used */
/* 0xc2 */
/* 0xc3 */
/* }}} */

/* {{{ MP_BIN */
/* 0xc4 */ /* MP_BIN(8)  */
/* 0xc5 */ /* MP_BIN(16) */
/* 0xc6 */ /* MP_BIN(32) */
/* }}} */

/* {{{ MP_EXT */
/* 0xc7 */
/* 0xc8 */
/* 0xc9 */
/* }}} */

/* {{{ MP_FLOAT, MP_DOUBLE */
/* 0xca */
/* 0xcb */
/* }}} */

/* {{{ MP_UINT */
/* 0xcc */
/* 0xcd */
/* 0xce */
/* 0xcf */
/* }}} */

/* {{{ MP_INT */
/* 0xd0 */ /* MP_INT (8)  */
/* 0xd1 */ /* MP_INT (16) */
/* 0xd2 */ /* MP_INT (32) */
/* 0xd3 */ /* MP_INT (64) */
/* }}} */

/* {{{ MP_EXT */
/* 0xd4 */ /* MP_INT (8)    */
/* 0xd5 */ /* MP_INT (16)   */
/* 0xd6 */ /* MP_INT (32)   */
/* 0xd7 */ /* MP_INT (64)   */
/* 0xd8 */ /* MP_INT (127)  */
/* }}} */

/* {{{ MP_STR */
/* 0xd9 */ /* MP_STR(8)  */
/* 0xda */ /* MP_STR(16) */
/* 0xdb */ /* MP_STR(32) */
/* }}} */

/* {{{ MP_ARRAY */
/* 0xdc */ /* MP_ARRAY(16)  */
/* 0xdd */ /* MP_ARRAY(32)  */
/* }}} */

/* {{{ MP_MAP */
/* 0xde */ /* MP_MAP (16) */
/* 0xdf */ /* MP_MAP (32) */
/* }}} */

/* {{{ MP_INT */
/* 0xe0 */
/* 0xe1 */
/* 0xe2 */
/* 0xe3 */
/* 0xe4 */
/* 0xe5 */
/* 0xe6 */
/* 0xe7 */
/* 0xe8 */
/* 0xe9 */
/* 0xea */
/* 0xeb */
/* 0xec */
/* 0xed */
/* 0xee */
/* 0xef */
/* 0xf0 */
/* 0xf1 */
/* 0xf2 */
/* 0xf3 */
/* 0xf4 */
/* 0xf5 */
/* 0xf6 */
/* 0xf7 */
/* 0xf8 */
/* 0xf9 */
/* 0xfa */
/* 0xfb */
/* 0xfc */
/* 0xfd */
/* 0xfe */
/* 0xff */
/* }}} */

/**
 * This lookup table used by mp_next() and mp_check() to determine
 * size of MsgPack element by its first byte.
 * A positive value contains size of the element (excluding the first byte).
 * A negative value means the element is compound (e.g. array or map)
 * of size (-n).
 * MP_HINT_* values used for special cases handled by switch() statement.
 */

/* {{{ MP_UINT(fixed) **/
/* 0x00 */
/* 0x01 */
/* 0x02 */
/* 0x03 */
/* 0x04 */
/* 0x05 */
/* 0x06 */
/* 0x07 */
/* 0x08 */
/* 0x09 */
/* 0x0a */
/* 0x0b */
/* 0x0c */
/* 0x0d */
/* 0x0e */
/* 0x0f */
/* 0x10 */
/* 0x11 */
/* 0x12 */
/* 0x13 */
/* 0x14 */
/* 0x15 */
/* 0x16 */
/* 0x17 */
/* 0x18 */
/* 0x19 */
/* 0x1a */
/* 0x1b */
/* 0x1c */
/* 0x1d */
/* 0x1e */
/* 0x1f */
/* 0x20 */
/* 0x21 */
/* 0x22 */
/* 0x23 */
/* 0x24 */
/* 0x25 */
/* 0x26 */
/* 0x27 */
/* 0x28 */
/* 0x29 */
/* 0x2a */
/* 0x2b */
/* 0x2c */
/* 0x2d */
/* 0x2e */
/* 0x2f */
/* 0x30 */
/* 0x31 */
/* 0x32 */
/* 0x33 */
/* 0x34 */
/* 0x35 */
/* 0x36 */
/* 0x37 */
/* 0x38 */
/* 0x39 */
/* 0x3a */
/* 0x3b */
/* 0x3c */
/* 0x3d */
/* 0x3e */
/* 0x3f */
/* 0x40 */
/* 0x41 */
/* 0x42 */
/* 0x43 */
/* 0x44 */
/* 0x45 */
/* 0x46 */
/* 0x47 */
/* 0x48 */
/* 0x49 */
/* 0x4a */
/* 0x4b */
/* 0x4c */
/* 0x4d */
/* 0x4e */
/* 0x4f */
/* 0x50 */
/* 0x51 */
/* 0x52 */
/* 0x53 */
/* 0x54 */
/* 0x55 */
/* 0x56 */
/* 0x57 */
/* 0x58 */
/* 0x59 */
/* 0x5a */
/* 0x5b */
/* 0x5c */
/* 0x5d */
/* 0x5e */
/* 0x5f */
/* 0x60 */
/* 0x61 */
/* 0x62 */
/* 0x63 */
/* 0x64 */
/* 0x65 */
/* 0x66 */
/* 0x67 */
/* 0x68 */
/* 0x69 */
/* 0x6a */
/* 0x6b */
/* 0x6c */
/* 0x6d */
/* 0x6e */
/* 0x6f */
/* 0x70 */
/* 0x71 */
/* 0x72 */
/* 0x73 */
/* 0x74 */
/* 0x75 */
/* 0x76 */
/* 0x77 */
/* 0x78 */
/* 0x79 */
/* 0x7a */
/* 0x7b */
/* 0x7c */
/* 0x7d */
/* 0x7e */
/* 0x7f */
/* }}} */

/* {{{ MP_MAP (fixed) */
/* 0x80 */ /* empty map - just skip one byte */
/* 0x81 */ /* 2 elements follow */
/* 0x82 */
/* 0x83 */
/* 0x84 */
/* 0x85 */
/* 0x86 */
/* 0x87 */
/* 0x88 */
/* 0x89 */
/* 0x8a */
/* 0x8b */
/* 0x8c */
/* 0x8d */
/* 0x8e */
/* 0x8f */
/* }}} */

/* {{{ MP_ARRAY (fixed) */
/* 0x90 */ /* empty array - just skip one byte */
/* 0x91 */ /* 1 element follows */
/* 0x92 */
/* 0x93 */
/* 0x94 */
/* 0x95 */
/* 0x96 */
/* 0x97 */
/* 0x98 */
/* 0x99 */
/* 0x9a */
/* 0x9b */
/* 0x9c */
/* 0x9d */
/* 0x9e */
/* 0x9f */
/* }}} */

/* {{{ MP_STR (fixed) */
/* 0xa0 */
/* 0xa1 */
/* 0xa2 */
/* 0xa3 */
/* 0xa4 */
/* 0xa5 */
/* 0xa6 */
/* 0xa7 */
/* 0xa8 */
/* 0xa9 */
/* 0xaa */
/* 0xab */
/* 0xac */
/* 0xad */
/* 0xae */
/* 0xaf */
/* 0xb0 */
/* 0xb1 */
/* 0xb2 */
/* 0xb3 */
/* 0xb4 */
/* 0xb5 */
/* 0xb6 */
/* 0xb7 */
/* 0xb8 */
/* 0xb9 */
/* 0xba */
/* 0xbb */
/* 0xbc */
/* 0xbd */
/* 0xbe */
/* 0xbf */
/* }}} */

/* {{{ MP_NIL, MP_BOOL */
/* 0xc0 */ /* MP_NIL */
/* 0xc1 */ /* never used */
/* 0xc2 */ /* MP_BOOL*/
/* 0xc3 */ /* MP_BOOL*/
/* }}} */

/* {{{ MP_BIN */
/* 0xc4 */ /* MP_BIN (8)  */
/* 0xc5 */ /* MP_BIN (16) */
/* 0xc6 */ /* MP_BIN (32) */
/* }}} */

/* {{{ MP_EXT */
/* 0xc7 */ /* MP_EXT (8)  */
/* 0xc8 */ /* MP_EXT (16) */
/* 0xc9 */ /* MP_EXT (32) */
/* }}} */

/* {{{ MP_FLOAT, MP_DOUBLE */
/* 0xca */ /* MP_FLOAT */
/* 0xcb */ /* MP_DOUBLE */
/* }}} */

/* {{{ MP_UINT */
/* 0xcc */ /* MP_UINT (8)  */
/* 0xcd */ /* MP_UINT (16) */
/* 0xce */ /* MP_UINT (32) */
/* 0xcf */ /* MP_UINT (64) */
/* }}} */

/* {{{ MP_INT */
/* 0xd0 */ /* MP_INT (8)  */
/* 0xd1 */ /* MP_INT (8)  */
/* 0xd2 */ /* MP_INT (8)  */
/* 0xd3 */ /* MP_INT (8)  */
/* }}} */

/* {{{ MP_EXT (fixext) */
/* 0xd4 */ /* MP_EXT (fixext 8)   */
/* 0xd5 */ /* MP_EXT (fixext 16)  */
/* 0xd6 */ /* MP_EXT (fixext 32)  */
/* 0xd7 */ /* MP_EXT (fixext 64)  */
/* 0xd8 */ /* MP_EXT (fixext 128) */
/* }}} */

/* {{{ MP_STR */
/* 0xd9 */ /* MP_STR (8) */
/* 0xda */ /* MP_STR (16) */
/* 0xdb */ /* MP_STR (32) */
/* }}} */

/* {{{ MP_ARRAY */
/* 0xdc */ /* MP_ARRAY (16) */
/* 0xdd */ /* MP_ARRAY (32) */
/* }}} */

/* {{{ MP_MAP */
/* 0xde */ /* MP_MAP (16) */
/* 0xdf */ /* MP_MAP (32) */
/* }}} */

/* {{{ MP_INT (fixed) */
/* 0xe0 */
/* 0xe1 */
/* 0xe2 */
/* 0xe3 */
/* 0xe4 */
/* 0xe5 */
/* 0xe6 */
/* 0xe7 */
/* 0xe8 */
/* 0xe9 */
/* 0xea */
/* 0xeb */
/* 0xec */
/* 0xed */
/* 0xee */
/* 0xef */
/* 0xf0 */
/* 0xf1 */
/* 0xf2 */
/* 0xf3 */
/* 0xf4 */
/* 0xf5 */
/* 0xf6 */
/* 0xf7 */
/* 0xf8 */
/* 0xf9 */
/* 0xfa */
/* 0xfb */
/* 0xfc */
/* 0xfd */
/* 0xfe */
/* 0xff */
/* }}} */

/* defined(MP_SOURCE) */

/** \endcond */

/*
 * }}}
 */

/* extern "C" */
/* defined(__cplusplus) */

/* MSGPUCK_H_INCLUDED */
