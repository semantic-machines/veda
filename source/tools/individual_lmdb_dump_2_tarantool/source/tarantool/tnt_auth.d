module veda.bind.tarantool.tnt_auth;

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
 * \file tnt_auth.h
 * \brief aithentication request
 */

/**
 * \brief Write authentication request to stream
 *
 * if user == NULL or user == "guest" then it'll be deauth
 *
 * \param s    stream instance
 * \param user user name
 * \param ulen user name length
 * \param pass password
 * \param plen password length
 *
 * \retval number of bytes written to stream
 */

ssize_t tnt_auth(
                 tnt_stream *s,
                 const(char) *user,
                 int ulen,
                 const(char) *pass,
                 int plen);

/**
 * \brief tnt_auth version with providing base64 encoded salt from tarantool
 */

ssize_t tnt_auth_raw(
                     tnt_stream *s,
                     const(char) *user,
                     int ulen,
                     const(char) *pass,
                     int plen,
                     const(char) *base64_salt);

/**
 * \brief Write deauth request to stream
 *
 * shortcut for tnt_auth(s, NULL, 0, NULL, 0)
 *
 * \param s stream instance
 *
 * \retval number of bytes written to stream
 */

ssize_t tnt_deauth(tnt_stream *s);

/* TNT_AUTH_H_INCLUDED */
