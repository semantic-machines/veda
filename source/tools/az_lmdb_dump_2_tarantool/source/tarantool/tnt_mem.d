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
 * \file tnt_mem.h
 * \brief Basic memory functions
 */

extern (D) auto TNT_GCC_VERSION(T0, T1) (auto ref T0 major, auto ref T1 minor)
{
    return __GNUC__ > major || (__GNUC__ == major && __GNUC_MINOR__ >= minor);
}

/* clang */

extern (D) auto tntlikely(T) (auto ref T x)
{
    return __builtin_expect(!!x, 1);
}

extern (D) auto tntunlikely(T) (auto ref T x)
{
    return __builtin_expect(!!x, 0);
}


/**
 * \brief basic allocation function type
 *
 * \param ptr  pointer to allocation/deallocation block
 * \param size size of block to allocat/reallocate
 *
 * \retval pointer to newly alloced/realloced block
 * \retval NULL on error/free
 */
alias void *function(void *ptr, size_t size) tnt_allocator_t;

/**
 * \brief initialize memory allocation function
 */
void *tnt_mem_init(void *function() alloc);

/**
 * \brief Internal function
 */
void *tnt_mem_alloc(size_t size);

/**
 * \brief Internal function
 */
void *tnt_mem_realloc(void *ptr, size_t size);

/**
 * \brief Internal function
 */
char *tnt_mem_dup(char *sz);

/**
 * \brief Internal function
 */
void tnt_mem_free(void *ptr);

/* TNT_MEM_H_INCLUDED */
