/**
 * обвязка к MDBX
 */
module veda.storage.mdbx.mdbx_header;

import core.sys.posix.sys.stat;

nothrow extern (C) {
version (Posix) {
    alias mode_t mdbx_mode_t;
    alias int    mdbx_filehandle_t;
}

alias uint  uint32_t;
alias ulong uint64_t;
alias       intptr_t = long;

alias void  MDBX_env;
alias void  MDBX_txn;
alias uint  MDBX_dbi;
alias void  MDBX_cursor;

struct iovec
{
    void   *iov_base;
    size_t iov_len;
};

/* Statistics for a database in the environment */
struct MDBX_stat
{
    uint32_t ms_psize;          /* Size of a database page.
                                 * This is currently the same for all databases. */
    uint32_t ms_depth;          /* Depth (height) of the B-tree */
    uint64_t ms_branch_pages;   /* Number of internal (non-leaf) pages */
    uint64_t ms_leaf_pages;     /* Number of leaf pages */
    uint64_t ms_overflow_pages; /* Number of overflow pages */
    uint64_t ms_entries;        /* Number of data items */
};

struct mi_geo_s
{
    uint64_t lower;   /* lower limit for datafile size */
    uint64_t upper;   /* upper limit for datafile size */
    uint64_t current; /* current datafile size */
    uint64_t shrink;  /* shrink theshold for datafile */
    uint64_t grow;    /* growth step for datafile */
};

struct MDBX_envinfo
{
    mi_geo_s mi_geo;
    uint64_t mi_mapsize;             /* Size of the data memory map */
    uint64_t mi_last_pgno;           /* ID of the last used page */
    uint64_t mi_recent_txnid;        /* ID of the last committed transaction */
    uint64_t mi_latter_reader_txnid; /* ID of the last reader transaction */
    uint64_t mi_meta0_txnid, mi_meta0_sign;
    uint64_t mi_meta1_txnid, mi_meta1_sign;
    uint64_t mi_meta2_txnid, mi_meta2_sign;
    uint32_t mi_maxreaders;   /* max reader slots in the environment */
    uint32_t mi_numreaders;   /* max reader slots used in the environment */
    uint32_t mi_dxb_pagesize; /* database pagesize */
    uint32_t mi_sys_pagesize; /* system pagesize */
};

alias iovec MDBX_val;

/* Environment Flags */

/* no environment directory */
const MDBX_NOSUBDIR = 0x4000u;
/* don't fsync after commit */
const MDBX_NOSYNC = 0x10000u;
/* read only */
const MDBX_RDONLY = 0x20000u;
/* don't fsync metapage after commit */
const MDBX_NOMETASYNC = 0x40000u;
/* use writable mmap */
const MDBX_WRITEMAP = 0x80000u;
/* use asynchronous msync when MDBX_WRITEMAP is used */
const MDBX_MAPASYNC = 0x100000u;
/* tie reader locktable slots to MDBX_txn objects instead of to threads */
const MDBX_NOTLS = 0x200000u;
/* don't do any locking, caller must manage their own locks
 * WARNING: libmdbx don't support this mode. */
const MDBX_NOLOCK__UNSUPPORTED = 0x400000u;
/* don't do readahead */
const MDBX_NORDAHEAD = 0x800000u;
/* don't initialize malloc'd memory before writing to datafile */
const MDBX_NOMEMINIT = 0x1000000u;
/* aim to coalesce FreeDB records */
const MDBX_COALESCE = 0x2000000u;
/* LIFO policy for reclaiming FreeDB records */
const MDBX_LIFORECLAIM = 0x4000000u;
/* make a steady-sync only on close and explicit env-sync */
const MDBX_UTTERLY_NOSYNC = (MDBX_NOSYNC | MDBX_MAPASYNC);
/* debuging option, fill/perturb released pages */
const MDBX_PAGEPERTURB = 0x8000000u;


/* Database Flags */
/* use reverse string keys */
const MDBX_REVERSEKEY = 0x02u;
/* use sorted duplicates */
const MDBX_DUPSORT = 0x04u;
/* numeric keys in native byte order, either uint32_t or uint64_t.
 * The keys must all be of the same size. */
const MDBX_INTEGERKEY = 0x08u;
/* with MDBX_DUPSORT, sorted dup items have fixed size */
const MDBX_DUPFIXED = 0x10u;
/* with MDBX_DUPSORT, dups are MDBX_INTEGERKEY-style integers */
const MDBX_INTEGERDUP = 0x20u;
/* with MDBX_DUPSORT, use reverse string dups */
const MDBX_REVERSEDUP = 0x40u;
/* create DB if not already existing */
const MDBX_CREATE = 0x40000u;


/* Write Flags */
/* For put: Don't write if the key already exists. */
const MDBX_NOOVERWRITE = 0x10u;
/* Only for MDBX_DUPSORT
 * For put: don't write if the key and data pair already exist.
 * For mdbx_cursor_del: remove all duplicate data items. */
const MDBX_NODUPDATA = 0x20u;
/* For mdbx_cursor_put: overwrite the current key/data pair
 * MDBX allows this flag for mdbx_put() for explicit overwrite/update without
 * insertion. */
const MDBX_CURRENT = 0x40u;
/* For put: Just reserve space for data, don't copy it. Return a
 * pointer to the reserved space. */
const MDBX_RESERVE = 0x10000u;
/* Data is being appended, don't split full pages. */
const MDBX_APPEND = 0x20000u;
/* Duplicate data is being appended, don't split full pages. */
const MDBX_APPENDDUP = 0x40000u;
/* Store multiple data items in one call. Only for MDBX_DUPFIXED. */
const MDBX_MULTIPLE = 0x80000u;

/* Transaction Flags */
/* Do not block when starting a write transaction */
const MDBX_TRYTXN = 0x10000000u;

/* Copy Flags */
/* Compacting copy: Omit free space from copy, and renumber all
 * pages sequentially. */
const MDBX_CP_COMPACT = 1u;

enum MDBX_cursor_op
{
    MDBX_FIRST,          /* Position at first key/data item */
    MDBX_FIRST_DUP,      /* MDBX_DUPSORT-only: Position at first data item
                          * of current key. */
    MDBX_GET_BOTH,       /* MDBX_DUPSORT-only: Position at key/data pair. */
    MDBX_GET_BOTH_RANGE, /* MDBX_DUPSORT-only: position at key, nearest data. */
    MDBX_GET_CURRENT,    /* Return key/data at current cursor position */
    MDBX_GET_MULTIPLE,   /* MDBX_DUPFIXED-only: Return key and up to a page of
                          * duplicate data items from current cursor position.
                          * Move cursor to prepare for MDBX_NEXT_MULTIPLE.*/
    MDBX_LAST,           /* Position at last key/data item */
    MDBX_LAST_DUP,       /* MDBX_DUPSORT-only: Position at last data item
                          * of current key. */
    MDBX_NEXT,           /* Position at next data item */
    MDBX_NEXT_DUP,       /* MDBX_DUPSORT-only: Position at next data item
                          * of current key. */
    MDBX_NEXT_MULTIPLE,  /* MDBX_DUPFIXED-only: Return key and up to a page of
                          * duplicate data items from next cursor position.
                          * Move cursor to prepare for MDBX_NEXT_MULTIPLE. */
    MDBX_NEXT_NODUP,     /* Position at first data item of next key */
    MDBX_PREV,           /* Position at previous data item */
    MDBX_PREV_DUP,       /* MDBX_DUPSORT-only: Position at previous data item
                          * of current key. */
    MDBX_PREV_NODUP,     /* Position at last data item of previous key */
    MDBX_SET,            /* Position at specified key */
    MDBX_SET_KEY,        /* Position at specified key, return both key and data */
    MDBX_SET_RANGE,      /* Position at first key greater than or equal to
                          * specified key. */
    MDBX_PREV_MULTIPLE   /* MDBX_DUPFIXED-only: Position at previous page and
                          * return key and up to a page of duplicate data items. */
};

/* Return Codes
 * BerkeleyDB uses -30800 to -30999, we'll go under them */

/* Successful result */
const MDBX_SUCCESS      = 0;
const MDBX_RESULT_FALSE = MDBX_SUCCESS;
const MDBX_RESULT_TRUE  = (-1);

/* key/data pair already exists */
const MDBX_KEYEXIST = (-30799);
/* key/data pair not found (EOF) */
const MDBX_NOTFOUND = (-30798);
/* Requested page not found - this usually indicates corruption */
const MDBX_PAGE_NOTFOUND = (-30797);
/* Located page was wrong type */
const MDBX_CORRUPTED = (-30796);
/* Update of meta page failed or environment had fatal error */
const MDBX_PANIC = (-30795);
/* DB file version mismatch with libmdbx */
const MDBX_VERSION_MISMATCH = (-30794);
/* File is not a valid MDBX file */
const MDBX_INVALID = (-30793);
/* Environment mapsize reached */
const MDBX_MAP_FULL = (-30792);
/* Environment maxdbs reached */
const MDBX_DBS_FULL = (-30791);
/* Environment maxreaders reached */
const MDBX_READERS_FULL = (-30790);
/* Txn has too many dirty pages */
const MDBX_TXN_FULL = (-30788);
/* Cursor stack too deep - internal error */
const MDBX_CURSOR_FULL = (-30787);
/* Page has not enough space - internal error */
const MDBX_PAGE_FULL = (-30786);
/* Database contents grew beyond environment mapsize */
const MDBX_MAP_RESIZED = (-30785);
/* Operation and DB incompatible, or DB type changed. This can mean:
*  - The operation expects an MDBX_DUPSORT / MDBX_DUPFIXED database.
*  - Opening a named DB when the unnamed DB has MDBX_DUPSORT/MDBX_INTEGERKEY.
*  - Accessing a data record as a database, or vice versa.
*  - The database was dropped and recreated with different flags. */
const MDBX_INCOMPATIBLE = (-30784);
/* Invalid reuse of reader locktable slot */
const MDBX_BAD_RSLOT = (-30783);
/* Transaction must abort, has a child, or is invalid */
const MDBX_BAD_TXN = (-30782);
/* Unsupported size of key/DB name/data, or wrong DUPFIXED size */
const MDBX_BAD_VALSIZE = (-30781);
/* The specified DBI was changed unexpectedly */
const MDBX_BAD_DBI = (-30780);
/* Unexpected problem - txn should abort */
const MDBX_PROBLEM = (-30779);
/* Another write transaction is running */
const MDBX_BUSY = (-30778);
/* The last defined error code */
const MDBX_LAST_ERRCODE = MDBX_BUSY;

/* The mdbx_put() or mdbx_replace() was called for key,
    that has more that one associated value. */
const MDBX_EMULTIVAL = (-30421);

/* Bad signature of a runtime object(s), this can mean:
 *  - memory corruption or double-free;
 *  - ABI version mismatch (rare case); */
const MDBX_EBADSIGN = (-30420);

/* Database should be recovered, but this could NOT be done automatically
 * right now (e.g. in readonly mode and so forth). */
const MDBX_WANNA_RECOVERY = (-30419);

/* The given key value is mismatched to the current cursor position,
 * when mdbx_cursor_put() called with MDBX_CURRENT option. */
const MDBX_EKEYMISMATCH = (-30418);

/* Database is too large for current system,
 * e.g. could NOT be mapped into RAM. */
const MDBX_TOO_LARGE = (-30417);

/* A thread has attempted to use a not owned object,
 * e.g. a transaction that started by another thread. */
const MDBX_THREAD_MISMATCH = (-30416);

/* Return a string describing a given error code.
 *
 * This function is a superset of the ANSI C X3.159-1989 (ANSI C) strerror(3)
 * function. If the error code is greater than or equal to 0, then the string
 * returned by the system function strerror(3) is returned. If the error code
 * is less than 0, an error string corresponding to the MDBX library error is
 * returned. See errors for a list of MDBX-specific error codes.
 *
 * [in] err The error code
 *
 * Returns "error message" The description of the error */
char *mdbx_strerror(int errnum);
char *mdbx_strerror_r(int errnum, char *buf, size_t buflen);

/* Create an MDBX environment handle.
 *
 * This function allocates memory for a MDBX_env structure. To release
 * the allocated memory and discard the handle, call mdbx_env_close().
 * Before the handle may be used, it must be opened using mdbx_env_open().
 * Various other options may also need to be set before opening the handle,
 * e.g. mdbx_env_set_mapsize(), mdbx_env_set_maxreaders(),
 * mdbx_env_set_maxdbs(), depending on usage requirements.
 *
 * [out] env The address where the new handle will be stored
 *
 * Returns A non-zero error value on failure and 0 on success. */
int mdbx_env_create(MDBX_env **penv);

/* Open an environment handle.
 *
 * If this function fails, mdbx_env_close() must be called to discard
 * the MDBX_env handle.
 *
 * [in] env    An environment handle returned by mdbx_env_create()
 * [in] path   The directory in which the database files reside.
 *             This directory must already exist and be writable.
 * [in] flags  Special options for this environment. This parameter
 *             must be set to 0 or by bitwise OR'ing together one
 *             or more of the values described here.
 *
 * Flags set by mdbx_env_set_flags() are also used:
 *  - MDBX_NOSUBDIR
 *      By default, MDBX creates its environment in a directory whose
 *      pathname is given in path, and creates its data and lock files
 *      under that directory. With this option, path is used as-is for
 *      the database main data file. The database lock file is the path
 *      with "-lock" appended.
 *
 *  - MDBX_RDONLY
 *      Open the environment in read-only mode. No write operations will
 *      be allowed. MDBX will still modify the lock file - except on
 *      read-only filesystems, where MDBX does not use locks.
 *
 *  - MDBX_WRITEMAP
 *      Use a writeable memory map unless MDBX_RDONLY is set. This uses fewer
 *      mallocs but loses protection from application bugs like wild pointer
 *      writes and other bad updates into the database.
 *      This may be slightly faster for DBs that fit entirely in RAM,
 *      but is slower for DBs larger than RAM.
 *      Incompatible with nested transactions.
 *      Do not mix processes with and without MDBX_WRITEMAP on the same
 *      environment.  This can defeat durability (mdbx_env_sync etc).
 *
 *  - MDBX_NOMETASYNC
 *      Flush system buffers to disk only once per transaction, omit the
 *      metadata flush. Defer that until the system flushes files to disk,
 *      or next non-MDBX_RDONLY commit or mdbx_env_sync(). This optimization
 *      maintains database integrity, but a system crash may undo the last
 *      committed transaction. I.e. it preserves the ACI (atomicity,
 *      consistency, isolation) but not D (durability) database property.
 *      This flag may be changed at any time using mdbx_env_set_flags().
 *
 *  - MDBX_NOSYNC
 *      Don't flush system buffers to disk when committing a transaction.
 *      This optimization means a system crash can corrupt the database or
 *      lose the last transactions if buffers are not yet flushed to disk.
 *      The risk is governed by how often the system flushes dirty buffers
 *      to disk and how often mdbx_env_sync() is called.  However, if the
 *      filesystem preserves write order and the MDBX_WRITEMAP and/or
 *      MDBX_LIFORECLAIM flags are not used, transactions exhibit ACI
 *      (atomicity, consistency, isolation) properties and only lose D
 *      (durability).  I.e. database integrity is maintained, but a system
 *      crash may undo the final transactions.
 *
 *      Note that (MDBX_NOSYNC | MDBX_WRITEMAP) leaves the system with no
 *      hint for when to write transactions to disk.
 *      Therefore the (MDBX_MAPASYNC | MDBX_WRITEMAP) may be preferable.
 *      This flag may be changed at any time using mdbx_env_set_flags().
 *
 *  - MDBX_UTTERLY_NOSYNC (internally MDBX_NOSYNC | MDBX_MAPASYNC)
 *      FIXME: TODO
 *
 *  - MDBX_MAPASYNC
 *      When using MDBX_WRITEMAP, use asynchronous flushes to disk. As with
 *      MDBX_NOSYNC, a system crash can then corrupt the database or lose
 *      the last transactions. Calling mdbx_env_sync() ensures on-disk
 *      database integrity until next commit. This flag may be changed at
 *      any time using mdbx_env_set_flags().
 *
 *  - MDBX_NOTLS
 *      Don't use Thread-Local Storage. Tie reader locktable slots to
 *      MDBX_txn objects instead of to threads. I.e. mdbx_txn_reset() keeps
 *      the slot reseved for the MDBX_txn object. A thread may use parallel
 *      read-only transactions. A read-only transaction may span threads if
 *      the user synchronizes its use. Applications that multiplex many
 *      user threads over individual OS threads need this option. Such an
 *      application must also serialize the write transactions in an OS
 *      thread, since MDBX's write locking is unaware of the user threads.
 *
 *  - MDBX_NOLOCK (don't supported by MDBX)
 *      Don't do any locking. If concurrent access is anticipated, the
 *      caller must manage all concurrency itself. For proper operation
 *      the caller must enforce single-writer semantics, and must ensure
 *      that no readers are using old transactions while a writer is
 *      active. The simplest approach is to use an exclusive lock so that
 *      no readers may be active at all when a writer begins.
 *
 *  - MDBX_NORDAHEAD
 *      Turn off readahead. Most operating systems perform readahead on
 *      read requests by default. This option turns it off if the OS
 *      supports it. Turning it off may help random read performance
 *      when the DB is larger than RAM and system RAM is full.
 *
 *  - MDBX_NOMEMINIT
 *      Don't initialize malloc'd memory before writing to unused spaces
 *      in the data file. By default, memory for pages written to the data
 *      file is obtained using malloc. While these pages may be reused in
 *      subsequent transactions, freshly malloc'd pages will be initialized
 *      to zeroes before use. This avoids persisting leftover data from other
 *      code (that used the heap and subsequently freed the memory) into the
 *      data file. Note that many other system libraries may allocate and free
 *      memory from the heap for arbitrary uses. E.g., stdio may use the heap
 *      for file I/O buffers. This initialization step has a modest performance
 *      cost so some applications may want to disable it using this flag. This
 *      option can be a problem for applications which handle sensitive data
 *      like passwords, and it makes memory checkers like Valgrind noisy. This
 *      flag is not needed with MDBX_WRITEMAP, which writes directly to the
 *      mmap instead of using malloc for pages. The initialization is also
 *      skipped if MDBX_RESERVE is used; the caller is expected to overwrite
 *      all of the memory that was reserved in that case. This flag may be
 *      changed at any time using mdbx_env_set_flags().
 *
 *  - MDBX_COALESCE
 *      Aim to coalesce records while reclaiming FreeDB. This flag may be
 *      changed at any time using mdbx_env_set_flags().
 *      FIXME: TODO
 *
 *  - MDBX_LIFORECLAIM
 *      LIFO policy for reclaiming FreeDB records. This significantly reduce
 *      write IPOs in case MDBX_NOSYNC with periodically checkpoints.
 *      FIXME: TODO
 *
 * [in] mode The UNIX permissions to set on created files.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *   - MDBX_VERSION_MISMATCH - the version of the MDBX library doesn't match the
 *                             version that created the database environment.
 *   - MDBX_INVALID  - the environment file headers are corrupted.
 *   - MDBX_ENOENT   - the directory specified by the path parameter
 *                     doesn't exist.
 *   - MDBX_EACCES   - the user didn't have permission to access
 *                     the environment files.
 *   - MDBX_EAGAIN   - the environment was locked by another process. */
int mdbx_env_open(MDBX_env *env, const char *path, uint flags,
                  mode_t mode);
int mdbx_env_open_ex(MDBX_env *env, const char *path,
                     uint flags, mode_t mode, int *exclusive);

/* Copy an MDBX environment to the specified path, with options.
*
* This function may be used to make a backup of an existing environment.
* No lockfile is created, since it gets recreated at need.
* NOTE: This call can trigger significant file size growth if run in
* parallel with write transactions, because it employs a read-only
* transaction. See long-lived transactions under "Caveats" section.
*
* [in] env    An environment handle returned by mdbx_env_create(). It must
*             have already been opened successfully.
* [in] path   The directory in which the copy will reside. This directory
*             must already exist and be writable but must otherwise be empty.
* [in] flags  Special options for this operation. This parameter must be set
*             to 0 or by bitwise OR'ing together one or more of the values
*             described here:
*
*  - MDBX_CP_COMPACT
*      Perform compaction while copying: omit free pages and sequentially
*      renumber all pages in output. This option consumes little bit more
*      CPU for processing, but may running quickly than the default, on
*      account skipping free pages.
*
*      NOTE: Currently it fails if the environment has suffered a page leak.
*
* Returns A non-zero error value on failure and 0 on success. */
int mdbx_env_copy(MDBX_env *env, const char *path, uint flags);

/* Copy an MDBX environment to the specified file descriptor,
 * with options.
 *
 * This function may be used to make a backup of an existing environment.
 * No lockfile is created, since it gets recreated at need. See
 * mdbx_env_copy() for further details.
 *
 * NOTE: This call can trigger significant file size growth if run in
 * parallel with write transactions, because it employs a read-only
 * transaction. See long-lived transactions under "Caveats" section.
 *
 * [in] env     An environment handle returned by mdbx_env_create(). It must
 *              have already been opened successfully.
 * [in] fd      The filedescriptor to write the copy to. It must have already
 *              been opened for Write access.
 * [in] flags   Special options for this operation. See mdbx_env_copy() for
 *              options.
 *
 * Returns A non-zero error value on failure and 0 on success. */
int mdbx_env_copy2fd(MDBX_env *env, mdbx_filehandle_t fd,
                     uint flags);

/* Return statistics about the MDBX environment.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [out] stat   The address of an MDBX_stat structure where the statistics
 *              will be copied */
int mdbx_env_stat(MDBX_env *env, MDBX_stat *stat, size_t bytes);

/* Return information about the MDBX environment.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [out] stat   The address of an MDBX_envinfo structure
 *              where the information will be copied */
int mdbx_env_info(MDBX_env *env, MDBX_envinfo *info, size_t bytes);

/* Flush the data buffers to disk.
 *
 * Data is always written to disk when mdbx_txn_commit() is called,
 * but the operating system may keep it buffered. MDBX always flushes
 * the OS buffers upon commit as well, unless the environment was
 * opened with MDBX_NOSYNC or in part MDBX_NOMETASYNC. This call is
 * not valid if the environment was opened with MDBX_RDONLY.
 *
 * [in] env   An environment handle returned by mdbx_env_create()
 * [in] force If non-zero, force a synchronous flush.  Otherwise if the
 *            environment has the MDBX_NOSYNC flag set the flushes will be
 *            omitted, and with MDBX_MAPASYNC they will be asynchronous.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EACCES   - the environment is read-only.
 *  - MDBX_EINVAL   - an invalid parameter was specified.
 *  - MDBX_EIO      - an error occurred during synchronization. */
int mdbx_env_sync(MDBX_env *env, int force);

/* Close the environment and release the memory map.
 *
 * Only a single thread may call this function. All transactions, databases,
 * and cursors must already be closed before calling this function. Attempts
 * to use any such handles after calling this function will cause a SIGSEGV.
 * The environment handle will be freed and must not be used again after this
 * call.
 *
 * [in] env        An environment handle returned by mdbx_env_create()
 * [in] dont_sync  A dont'sync flag, if non-zero the last checkpoint (meta-page
 *                 update) will be kept "as is" and may be still "weak" in the
 *                 NOSYNC/MAPASYNC modes. Such "weak" checkpoint will be
 *                 ignored on opening next time, and transactions since the
 *                 last non-weak checkpoint (meta-page update) will rolledback
 *                 for consistency guarantee. */
int mdbx_env_close(MDBX_env *env);

/* Set environment flags.
 *
 * This may be used to set some flags in addition to those from
 * mdbx_env_open(), or to unset these flags.  If several threads
 * change the flags at the same time, the result is undefined.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [in] flags   The flags to change, bitwise OR'ed together
 * [in] onoff   A non-zero value sets the flags, zero clears them.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_env_set_flags(MDBX_env *env, uint flags, int onoff);

/* Get environment flags.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [out] flags  The address of an integer to store the flags
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_env_get_flags(MDBX_env *env, uint *flags);

/* Return the path that was used in mdbx_env_open().
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [out] path   Address of a string pointer to contain the path.
 *              This is the actual string in the environment, not a copy.
 *              It should not be altered in any way.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_env_get_path(MDBX_env *env, const char **path);

/* Return the file descriptor for the given environment.
 *
 * NOTE: All MDBX file descriptors have FD_CLOEXEC and
 * could't be used after exec() and or fork().
 *
 * [in] env   An environment handle returned by mdbx_env_create()
 * [out] fd   Address of a int to contain the descriptor.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_env_get_fd(MDBX_env *env, mdbx_filehandle_t *fd);

/* Set the size of the memory map to use for this environment.
 *
 * The size should be a multiple of the OS page size. The default is
 * 10485760 bytes. The size of the memory map is also the maximum size
 * of the database. The value should be chosen as large as possible,
 * to accommodate future growth of the database.
 * This function should be called after mdbx_env_create() and before
 * mdbx_env_open(). It may be called at later times if no transactions
 * are active in this process. Note that the library does not check for
 * this condition, the caller must ensure it explicitly.
 *
 * The new size takes effect immediately for the current process but
 * will not be persisted to any others until a write transaction has been
 * committed by the current process. Also, only mapsize increases are
 * persisted into the environment.
 *
 * If the mapsize is increased by another process, and data has grown
 * beyond the range of the current mapsize, mdbx_txn_begin() will
 * return MDBX_MAP_RESIZED. This function may be called with a size
 * of zero to adopt the new size.
 *
 * Any attempt to set a size smaller than the space already consumed by the
 * environment will be silently changed to the current size of the used space.
 *
 * [in] env   An environment handle returned by mdbx_env_create()
 * [in] size  The size in bytes
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified,
 *                    or the environment has an active write transaction. */
int mdbx_env_set_mapsize(MDBX_env *env, size_t size);
int mdbx_env_set_geometry(MDBX_env *env, intptr_t size_lower,
                          intptr_t size_now, intptr_t size_upper,
                          intptr_t growth_step,
                          intptr_t shrink_threshold,
                          intptr_t pagesize);

/* Set the maximum number of threads/reader slots for the environment.
 *
 * This defines the number of slots in the lock table that is used to track
 * readers in the the environment. The default is 61.
 * Starting a read-only transaction normally ties a lock table slot to the
 * current thread until the environment closes or the thread exits. If
 * MDBX_NOTLS is in use, mdbx_txn_begin() instead ties the slot to the
 * MDBX_txn object until it or the MDBX_env object is destroyed.
 * This function may only be called after mdbx_env_create() and before
 * mdbx_env_open().
 *
 * [in] env       An environment handle returned by mdbx_env_create()
 * [in] readers   The maximum number of reader lock table slots
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified,
 *                    or the environment is already open. */
int mdbx_env_set_maxreaders(MDBX_env *env, uint readers);

/* Get the maximum number of threads/reader slots for the environment.
 *
 * [in] env An environment handle returned by mdbx_env_create()
 * [out] readers Address of an integer to store the number of readers
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_env_get_maxreaders(MDBX_env *env, uint *readers);

/* Set the maximum number of named databases for the environment.
 *
 * This function is only needed if multiple databases will be used in the
 * environment. Simpler applications that use the environment as a single
 * unnamed database can ignore this option.
 * This function may only be called after mdbx_env_create() and before
 * mdbx_env_open().
 *
 * Currently a moderate number of slots are cheap but a huge number gets
 * expensive: 7-120 words per transaction, and every mdbx_dbi_open()
 * does a linear search of the opened slots.
 *
 * [in] env   An environment handle returned by mdbx_env_create()
 * [in] dbs   The maximum number of databases
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified,
 *                    or the environment is already open. */
int mdbx_env_set_maxdbs(MDBX_env *env, MDBX_dbi dbs);

/* Get the maximum size of keys and MDBX_DUPSORT data we can write.
 *
 * [in] env  An environment handle returned by mdbx_env_create()
 *
 * Returns The maximum size of a key we can write. */
int mdbx_env_get_maxkeysize(MDBX_env *env);
int mdbx_get_maxkeysize(size_t pagesize);

/* Set application information associated with the MDBX_env.
 *
 * [in] env  An environment handle returned by mdbx_env_create()
 * [in] ctx  An arbitrary pointer for whatever the application needs.
 *
 * Returns A non-zero error value on failure and 0 on success. */
int mdbx_env_set_userctx(MDBX_env *env, void *ctx);

/* Get the application information associated with the MDBX_env.
 *
 * [in] env An environment handle returned by mdbx_env_create()
 * Returns The pointer set by mdbx_env_set_userctx(). */
void *mdbx_env_get_userctx(MDBX_env *env);

/* Create a transaction for use with the environment.
 *
 * The transaction handle may be discarded using mdbx_txn_abort()
 * or mdbx_txn_commit().
 * NOTE: A transaction and its cursors must only be used by a single
 * thread, and a thread may only have a single transaction at a time.
 * If MDBX_NOTLS is in use, this does not apply to read-only transactions.
 * NOTE: Cursors may not span transactions.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [in] parent  If this parameter is non-NULL, the new transaction will be
 *              a nested transaction, with the transaction indicated by parent
 *              as its parent. Transactions may be nested to any level.
 *              A parent transaction and its cursors may not issue any other
 *              operations than mdbx_txn_commit and mdbx_txn_abort while it
 *              has active child transactions.
 * [in] flags   Special options for this transaction. This parameter
 *              must be set to 0 or by bitwise OR'ing together one or more
 *              of the values described here.
 *
 *  - MDBX_RDONLY
 *      This transaction will not perform any write operations.
 *
 *  - MDBX_TRYTXN
 *      Do not block when starting a write transaction
 *
 * [out] txn Address where the new MDBX_txn handle will be stored
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_PANIC         - a fatal error occurred earlier and the environment
 *                        must be shut down.
 *  - MDBX_MAP_RESIZED   - another process wrote data beyond this MDBX_env's
 *                         mapsize and this environment's map must be resized
 *                         as well. See mdbx_env_set_mapsize().
 *  - MDBX_READERS_FULL  - a read-only transaction was requested and the reader
 *                         lock table is full. See mdbx_env_set_maxreaders().
 *  - MDBX_ENOMEM        - out of memory.
 *  - MDBX_BUSY          - a write transaction is already started. */
int mdbx_txn_begin(MDBX_env *env, MDBX_txn *parent, uint flags,
                   MDBX_txn **txn);

/* Returns the transaction's MDBX_env
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin() */
MDBX_env *mdbx_txn_env(MDBX_txn *txn);

/* Return the transaction's ID.
 *
 * This returns the identifier associated with this transaction. For a
 * read-only transaction, this corresponds to the snapshot being read;
 * concurrent readers will frequently have the same transaction ID.
 *
 * [in] txn A transaction handle returned by mdbx_txn_begin()
 *
 * Returns A transaction ID, valid if input is an active transaction. */
uint64_t mdbx_txn_id(MDBX_txn *txn);

/* Commit all the operations of a transaction into the database.
 *
 * The transaction handle is freed. It and its cursors must not be used
 * again after this call, except with mdbx_cursor_renew().
 *
 * A cursor must be closed explicitly always, before
 * or after its transaction ends. It can be reused with
 * mdbx_cursor_renew() before finally closing it.
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin()
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified.
 *  - MDBX_ENOSPC   - no more disk space.
 *  - MDBX_EIO      - a low-level I/O error occurred while writing.
 *  - MDBX_ENOMEM   - out of memory. */
int mdbx_txn_commit(MDBX_txn *txn);

/* Abandon all the operations of the transaction instead of saving them.
 *
 * The transaction handle is freed. It and its cursors must not be used
 * again after this call, except with mdbx_cursor_renew().
 *
 * A cursor must be closed explicitly always, before or after its transaction
 * ends. It can be reused with mdbx_cursor_renew() before finally closing it.
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin(). */
int mdbx_txn_abort(MDBX_txn *txn);

/* Reset a read-only transaction.
 *
 * Abort the transaction like mdbx_txn_abort(), but keep the transaction
 * handle. Therefore mdbx_txn_renew() may reuse the handle. This saves
 * allocation overhead if the process will start a new read-only transaction
 * soon, and also locking overhead if MDBX_NOTLS is in use. The reader table
 * lock is released, but the table slot stays tied to its thread or
 * MDBX_txn. Use mdbx_txn_abort() to discard a reset handle, and to free
 * its lock table slot if MDBX_NOTLS is in use.
 *
 * Cursors opened within the transaction must not be used
 * again after this call, except with mdbx_cursor_renew().
 *
 * Reader locks generally don't interfere with writers, but they keep old
 * versions of database pages allocated. Thus they prevent the old pages
 * from being reused when writers commit new data, and so under heavy load
 * the database size may grow much more rapidly than otherwise.
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin() */
int mdbx_txn_reset(MDBX_txn *txn);

/* Renew a read-only transaction.
 *
 * This acquires a new reader lock for a transaction handle that had been
 * released by mdbx_txn_reset(). It must be called before a reset transaction
 * may be used again.
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin()
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_PANIC     - a fatal error occurred earlier and the environment
 *                     must be shut down.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_txn_renew(MDBX_txn *txn);

/* Open a table in the environment.
 *
 * A table handle denotes the name and parameters of a table, independently
 * of whether such a table exists. The table handle may be discarded by
 * calling mdbx_dbi_close(). The old table handle is returned if the table
 * was already open. The handle may only be closed once.
 *
 * The table handle will be private to the current transaction until
 * the transaction is successfully committed. If the transaction is
 * aborted the handle will be closed automatically.
 * After a successful commit the handle will reside in the shared
 * environment, and may be used by other transactions.
 *
 * This function must not be called from multiple concurrent
 * transactions in the same process. A transaction that uses
 * this function must finish (either commit or abort) before
 * any other transaction in the process may use this function.
 *
 * To use named table (with name != NULL), mdbx_env_set_maxdbs()
 * must be called before opening the environment. Table names are
 * keys in the internal unnamed table, and may be read but not written.
 *
 * [in] txn    transaction handle returned by mdbx_txn_begin()
 * [in] name   The name of the table to open. If only a single
 *             table is needed in the environment, this value may be NULL.
 * [in] flags  Special options for this table. This parameter must be set
 *             to 0 or by bitwise OR'ing together one or more of the values
 *             described here:
 *  - MDBX_REVERSEKEY
 *      Keys are strings to be compared in reverse order, from the end
 *      of the strings to the beginning. By default, Keys are treated as
 *      strings and compared from beginning to end.
 *  - MDBX_DUPSORT
 *      Duplicate keys may be used in the table. Or, from another point of
 *      view, keys may have multiple data items, stored in sorted order. By
 *      default keys must be unique and may have only a single data item.
 *  - MDBX_INTEGERKEY
 *      Keys are binary integers in native byte order, either uin32_t or
 *      uint64_t, and will be sorted as such. The keys must all be of the
 *      same size.
 *  - MDBX_DUPFIXED
 *      This flag may only be used in combination with MDBX_DUPSORT. This
 *      option tells the library that the data items for this database are
 *      all the same size, which allows further optimizations in storage and
 *      retrieval. When all data items are the same size, the MDBX_GET_MULTIPLE,
 *      MDBX_NEXT_MULTIPLE and MDBX_PREV_MULTIPLE cursor operations may be used
 *      to retrieve multiple items at once.
 *  - MDBX_INTEGERDUP
 *      This option specifies that duplicate data items are binary integers,
 *      similar to MDBX_INTEGERKEY keys.
 *  - MDBX_REVERSEDUP
 *      This option specifies that duplicate data items should be compared as
 *      strings in reverse order (the comparison is performed in the direction
 *      from the last byte to the first).
 *  - MDBX_CREATE
 *      Create the named database if it doesn't exist. This option is not
 *      allowed in a read-only transaction or a read-only environment.
 *
 * [out]  dbi Address where the new MDBX_dbi handle will be stored
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_NOTFOUND  - the specified database doesn't exist in the
 *                     environment and MDBX_CREATE was not specified.
 *  - MDBX_DBS_FULL  - too many databases have been opened.
 *                     See mdbx_env_set_maxdbs(). */
//int mdbx_dbi_open_ex(MDBX_txn *txn, const char *name,
//                                 uint flags, MDBX_dbi *dbi,
//                                 MDBX_cmp_func *keycmp, MDBX_cmp_func *datacmp);
int mdbx_dbi_open(MDBX_txn *txn, const char *name, uint flags,
                  MDBX_dbi *dbi);

/* Retrieve statistics for a database.
 *
 * [in] txn     A transaction handle returned by mdbx_txn_begin()
 * [in] dbi     A database handle returned by mdbx_dbi_open()
 * [out] stat   The address of an MDBX_stat structure where the statistics
 *              will be copied
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_dbi_stat(MDBX_txn *txn, MDBX_dbi dbi, MDBX_stat *stat,
                  size_t bytes);

/* Retrieve the DB flags for a database handle.
 *
 * [in] txn     A transaction handle returned by mdbx_txn_begin()
 * [in] dbi     A database handle returned by mdbx_dbi_open()
 * [out] flags  Address where the flags will be returned.
 * [out] state  Address where the state will be returned.
 *
 * Returns A non-zero error value on failure and 0 on success. */
const MDBX_TBL_DIRTY = 0x01; /* DB was written in this txn */
const MDBX_TBL_STALE = 0x02; /* Named-DB record is older than txnID */
const MDBX_TBL_NEW   = 0x04; /* Named-DB handle opened in this txn */
int mdbx_dbi_flags_ex(MDBX_txn *txn, MDBX_dbi dbi, uint *flags,
                      uint *state);
int mdbx_dbi_flags(MDBX_txn *txn, MDBX_dbi dbi, uint *flags);

/* Close a database handle. Normally unnecessary.
 *
 * Use with care:
 * FIXME: This call is not mutex protected. Handles should only be closed by
 * a single thread, and only if no other threads are going to reference
 * the database handle or one of its cursors any further. Do not close
 * a handle if an existing transaction has modified its database.
 * Doing so can cause misbehavior from database corruption to errors
 * like MDBX_BAD_VALSIZE (since the DB name is gone).
 *
 * Closing a database handle is not necessary, but lets mdbx_dbi_open()
 * reuse the handle value.  Usually it's better to set a bigger
 * mdbx_env_set_maxdbs(), unless that value would be large.
 *
 * [in] env  An environment handle returned by mdbx_env_create()
 * [in] dbi  A database handle returned by mdbx_dbi_open()
 */
int mdbx_dbi_close(MDBX_env *env, MDBX_dbi dbi);

/* Empty or delete+close a database.
 *
 * See mdbx_dbi_close() for restrictions about closing the DB handle.
 *
 * [in] txn  A transaction handle returned by mdbx_txn_begin()
 * [in] dbi  A database handle returned by mdbx_dbi_open()
 * [in] del  0 to empty the DB, 1 to delete it from the environment
 *           and close the DB handle.
 *
 * Returns A non-zero error value on failure and 0 on success. */
int mdbx_drop(MDBX_txn *txn, MDBX_dbi dbi, int del);

/* Get items from a database.
 *
 * This function retrieves key/data pairs from the database. The address
 * and length of the data associated with the specified key are returned
 * in the structure to which data refers.
 * If the database supports duplicate keys (MDBX_DUPSORT) then the
 * first data item for the key will be returned. Retrieval of other
 * items requires the use of mdbx_cursor_get().
 *
 * NOTE: The memory pointed to by the returned values is owned by the
 * database. The caller need not dispose of the memory, and may not
 * modify it in any way. For values returned in a read-only transaction
 * any modification attempts will cause a SIGSEGV.
 *
 * NOTE: Values returned from the database are valid only until a
 * subsequent update operation, or the end of the transaction.
 *
 * [in] txn       A transaction handle returned by mdbx_txn_begin()
 * [in] dbi       A database handle returned by mdbx_dbi_open()
 * [in] key       The key to search for in the database
 * [in,out] data  The data corresponding to the key
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_NOTFOUND  - the key was not in the database.
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_get(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
             MDBX_val *data);

/* Store items into a database.
 *
 * This function stores key/data pairs in the database. The default behavior
 * is to enter the new key/data pair, replacing any previously existing key
 * if duplicates are disallowed, or adding a duplicate data item if
 * duplicates are allowed (MDBX_DUPSORT).
 *
 * [in] txn    A transaction handle returned by mdbx_txn_begin()
 * [in] dbi    A database handle returned by mdbx_dbi_open()
 * [in] key    The key to store in the database
 * [in,out]    data The data to store
 * [in] flags  Special options for this operation. This parameter must be
 *             set to 0 or by bitwise OR'ing together one or more of the
 *             values described here.
 *
 *  - MDBX_NODUPDATA
 *      Enter the new key/data pair only if it does not already appear
 *      in the database. This flag may only be specified if the database
 *      was opened with MDBX_DUPSORT. The function will return MDBX_KEYEXIST
 *      if the key/data pair already appears in the database.
 *
 *  - MDBX_NOOVERWRITE
 *      Enter the new key/data pair only if the key does not already appear
 *      in the database. The function will return MDBX_KEYEXIST if the key
 *      already appears in the database, even if the database supports
 *      duplicates (MDBX_DUPSORT). The data parameter will be set to point
 *      to the existing item.
 *
 *  - MDBX_CURRENT
 *      Update an single existing entry, but not add new ones. The function
 *      will return MDBX_NOTFOUND if the given key not exist in the database.
 *      Or the MDBX_EMULTIVAL in case duplicates for the given key.
 *
 *  - MDBX_RESERVE
 *      Reserve space for data of the given size, but don't copy the given
 *      data. Instead, return a pointer to the reserved space, which the
 *      caller can fill in later - before the next update operation or the
 *      transaction ends. This saves an extra memcpy if the data is being
 *      generated later. MDBX does nothing else with this memory, the caller
 *      is expected to modify all of the space requested. This flag must not
 *      be specified if the database was opened with MDBX_DUPSORT.
 *
 *  - MDBX_APPEND
 *      Append the given key/data pair to the end of the database. This option
 *      allows fast bulk loading when keys are already known to be in the
 *      correct order. Loading unsorted keys with this flag will cause
 *      a MDBX_EKEYMISMATCH error.
 *
 *  - MDBX_APPENDDUP
 *      As above, but for sorted dup data.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_KEYEXIST
 *  - MDBX_MAP_FULL  - the database is full, see mdbx_env_set_mapsize().
 *  - MDBX_TXN_FULL  - the transaction has too many dirty pages.
 *  - MDBX_EACCES    - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_put(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
             MDBX_val *data, uint flags);

/* Delete items from a database.
 *
 * This function removes key/data pairs from the database.
 *
 * The data parameter is NOT ignored regardless the database does
 * support sorted duplicate data items or not. If the data parameter
 * is non-NULL only the matching data item will be deleted.
 *
 * This function will return MDBX_NOTFOUND if the specified key/data
 * pair is not in the database.
 *
 * [in] txn   A transaction handle returned by mdbx_txn_begin()
 * [in] dbi   A database handle returned by mdbx_dbi_open()
 * [in] key   The key to delete from the database
 * [in] data  The data to delete
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EACCES   - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_del(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
             MDBX_val *data);

/* Create a cursor handle.
 *
 * A cursor is associated with a specific transaction and database.
 * A cursor cannot be used when its database handle is closed.  Nor
 * when its transaction has ended, except with mdbx_cursor_renew().
 * It can be discarded with mdbx_cursor_close().
 *
 * A cursor must be closed explicitly always, before
 * or after its transaction ends. It can be reused with
 * mdbx_cursor_renew() before finally closing it.
 *
 * [in] txn      A transaction handle returned by mdbx_txn_begin()
 * [in] dbi      A database handle returned by mdbx_dbi_open()
 * [out] cursor  Address where the new MDBX_cursor handle will be stored
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_cursor_open(MDBX_txn *txn, MDBX_dbi dbi,
                     MDBX_cursor **cursor);

/* Close a cursor handle.
 *
 * The cursor handle will be freed and must not be used again after this call.
 * Its transaction must still be live if it is a write-transaction.
 *
 * [in] cursor  A cursor handle returned by mdbx_cursor_open() */
void mdbx_cursor_close(MDBX_cursor *cursor);

/* Renew a cursor handle.
 *
 * A cursor is associated with a specific transaction and database.
 * Cursors that are only used in read-only transactions may be re-used,
 * to avoid unnecessary malloc/free overhead. The cursor may be associated
 * with a new read-only transaction, and referencing the same database handle
 * as it was created with.
 *
 * This may be done whether the previous transaction is live or dead.
 * [in] txn     A transaction handle returned by mdbx_txn_begin()
 * [in] cursor  A cursor handle returned by mdbx_cursor_open()
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_cursor_renew(MDBX_txn *txn, MDBX_cursor *cursor);

/* Return the cursor's transaction handle.
 *
 * [in] cursor A cursor handle returned by mdbx_cursor_open() */
MDBX_txn *mdbx_cursor_txn(MDBX_cursor *cursor);

/* Return the cursor's database handle.
 *
 * [in] cursor  A cursor handle returned by mdbx_cursor_open() */
MDBX_dbi mdbx_cursor_dbi(MDBX_cursor *cursor);

/* Retrieve by cursor.
 *
 * This function retrieves key/data pairs from the database. The address and
 * length of the key are returned in the object to which key refers (except
 * for the case of the MDBX_SET option, in which the key object is unchanged),
 * and the address and length of the data are returned in the object to which
 * data refers. See mdbx_get() for restrictions on using the output values.
 *
 * [in] cursor    A cursor handle returned by mdbx_cursor_open()
 * [in,out] key   The key for a retrieved item
 * [in,out] data  The data of a retrieved item
 * [in] op        A cursor operation MDBX_cursor_op
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_NOTFOUND  - no matching key found.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_cursor_get(MDBX_cursor *cursor, MDBX_val *key,
                    MDBX_val *data, MDBX_cursor_op op);

/* Store by cursor.
 *
 * This function stores key/data pairs into the database. The cursor is
 * positioned at the new item, or on failure usually near it.
 *
 * [in] cursor  A cursor handle returned by mdbx_cursor_open()
 * [in] key     The key operated on.
 * [in] data    The data operated on.
 * [in] flags   Options for this operation. This parameter
 *              must be set to 0 or one of the values described here:
 *
 *  - MDBX_CURRENT
 *      Replace the item at the current cursor position. The key parameter
 *      must still be provided, and must match it, otherwise the function
 *      return MDBX_EKEYMISMATCH.
 *
 *      NOTE: MDBX unlike LMDB allows you to change the size of the data and
 *      automatically handles reordering for sorted duplicates (MDBX_DUPSORT).
 *
 *  - MDBX_NODUPDATA
 *      Enter the new key/data pair only if it does not already appear in the
 *      database. This flag may only be specified if the database was opened
 *      with MDBX_DUPSORT. The function will return MDBX_KEYEXIST if the
 *      key/data pair already appears in the database.
 *
 *  - MDBX_NOOVERWRITE
 *      Enter the new key/data pair only if the key does not already appear
 *      in the database. The function will return MDBX_KEYEXIST if the key
 *      already appears in the database, even if the database supports
 *      duplicates (MDBX_DUPSORT).
 *
 *  - MDBX_RESERVE
 *      Reserve space for data of the given size, but don't copy the given
 *      data. Instead, return a pointer to the reserved space, which the
 *      caller can fill in later - before the next update operation or the
 *      transaction ends. This saves an extra memcpy if the data is being
 *      generated later. This flag must not be specified if the database
 *      was opened with MDBX_DUPSORT.
 *
 *  - MDBX_APPEND
 *      Append the given key/data pair to the end of the database. No key
 *      comparisons are performed. This option allows fast bulk loading when
 *      keys are already known to be in the correct order. Loading unsorted
 *      keys with this flag will cause a MDBX_KEYEXIST error.
 *
 *  - MDBX_APPENDDUP
 *      As above, but for sorted dup data.
 *
 *  - MDBX_MULTIPLE
 *      Store multiple contiguous data elements in a single request. This flag
 *      may only be specified if the database was opened with MDBX_DUPFIXED.
 *      The data argument must be an array of two MDBX_vals. The iov_len of the
 *      first MDBX_val must be the size of a single data element. The iov_base
 *      of the first MDBX_val must point to the beginning of the array of
 *      contiguous data elements. The iov_len of the second MDBX_val must be
 *      the count of the number of data elements to store. On return this
 *      field will be set to the count of the number of elements actually
 *      written. The iov_base of the second MDBX_val is unused.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EKEYMISMATCH
 *  - MDBX_MAP_FULL  - the database is full, see mdbx_env_set_mapsize().
 *  - MDBX_TXN_FULL  - the transaction has too many dirty pages.
 *  - MDBX_EACCES    - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_cursor_put(MDBX_cursor *cursor, MDBX_val *key,
                    MDBX_val *data, uint flags);

/* Delete current key/data pair
 *
 * This function deletes the key/data pair to which the cursor refers.
 *
 * [in] cursor  A cursor handle returned by mdbx_cursor_open()
 * [in] flags   Options for this operation. This parameter must be set to 0
 *              or one of the values described here.
 *
 *  - MDBX_NODUPDATA
 *      Delete all of the data items for the current key. This flag may only
 *      be specified if the database was opened with MDBX_DUPSORT.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EACCES  - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL  - an invalid parameter was specified. */
int mdbx_cursor_del(MDBX_cursor *cursor, uint flags);

/* Return count of duplicates for current key.
 *
 * This call is only valid on databases that support sorted duplicate data
 * items MDBX_DUPSORT.
 *
 * [in] cursor    A cursor handle returned by mdbx_cursor_open()
 * [out] countp   Address where the count will be stored
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EINVAL   - cursor is not initialized, or an invalid parameter
 *                    was specified. */
int mdbx_cursor_count(MDBX_cursor *cursor, size_t *countp);

/* Compare two data items according to a particular database.
 *
 * This returns a comparison as if the two data items were keys in the
 * specified database.
 *
 * [in] txn   A transaction handle returned by mdbx_txn_begin()
 * [in] dbi   A database handle returned by mdbx_dbi_open()
 * [in] a     The first item to compare
 * [in] b     The second item to compare
 *
 * Returns < 0 if a < b, 0 if a == b, > 0 if a > b */
int mdbx_cmp(MDBX_txn *txn, MDBX_dbi dbi, const MDBX_val *a,
             const MDBX_val *b);

/* Compare two data items according to a particular database.
 *
 * This returns a comparison as if the two items were data items of
 * the specified database. The database must have the MDBX_DUPSORT flag.
 *
 * [in] txn   A transaction handle returned by mdbx_txn_begin()
 * [in] dbi   A database handle returned by mdbx_dbi_open()
 * [in] a     The first item to compare
 * [in] b     The second item to compare
 *
 * Returns < 0 if a < b, 0 if a == b, > 0 if a > b */
int mdbx_dcmp(MDBX_txn *txn, MDBX_dbi dbi, const MDBX_val *a,
              const MDBX_val *b);

/* Check for stale entries in the reader lock table.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [out] dead   Number of stale slots that were cleared
 *
 * Returns 0 on success, non-zero on failure. */
int mdbx_reader_check(MDBX_env *env, int *dead);

char *mdbx_dkey(MDBX_val *key, char *buf, size_t bufsize);

int mdbx_env_close_ex(MDBX_env *env, int dont_sync);

/* Set threshold to force flush the data buffers to disk,
 * even of MDBX_NOSYNC, MDBX_NOMETASYNC and MDBX_MAPASYNC flags
 * in the environment.
 *
 * Data is always written to disk when mdbx_txn_commit() is called,
 * but the operating system may keep it buffered. MDBX always flushes
 * the OS buffers upon commit as well, unless the environment was
 * opened with MDBX_NOSYNC or in part MDBX_NOMETASYNC.
 *
 * The default is 0, than mean no any threshold checked, and no additional
 * flush will be made.
 *
 * [in] env     An environment handle returned by mdbx_env_create()
 * [in] bytes   The size in bytes of summary changes when a synchronous
 *              flush would be made.
 *
 * Returns A non-zero error value on failure and 0 on success. */
int mdbx_env_set_syncbytes(MDBX_env *env, size_t bytes);

/* Returns a lag of the reading for the given transaction.
 *
 * Returns an information for estimate how much given read-only
 * transaction is lagging relative the to actual head.
 *
 * [in] txn       A transaction handle returned by mdbx_txn_begin()
 * [out] percent  Percentage of page allocation in the database.
 *
 * Returns Number of transactions committed after the given was started for
 * read, or -1 on failure. */
int mdbx_txn_straggler(MDBX_txn *txn, int *percent);

/* Returns:
 *  - MDBX_RESULT_TRUE
 *      when no more data available or cursor not positioned;
 *  - MDBX_RESULT_FALSE
 *      when data available;
 *  - Otherwise the error code. */
int mdbx_cursor_eof(MDBX_cursor *mc);

/* Returns: MDBX_RESULT_TRUE, MDBX_RESULT_FALSE or Error code. */
int mdbx_cursor_on_first(MDBX_cursor *mc);

/* Returns: MDBX_RESULT_TRUE, MDBX_RESULT_FALSE or Error code. */
int mdbx_cursor_on_last(MDBX_cursor *mc);

int mdbx_replace(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
                 MDBX_val *new_data, MDBX_val *old_data,
                 uint flags);
/* Same as mdbx_get(), but:
 * 1) if values_count is not NULL, then returns the count
 *    of multi-values/duplicates for a given key.
 * 2) updates the key for pointing to the actual key's data inside DB. */
int mdbx_get_ex(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
                MDBX_val *data, size_t *values_count);

int mdbx_is_dirty(const MDBX_txn *txn, const void *ptr);

int mdbx_dbi_sequence(MDBX_txn *txn, MDBX_dbi dbi, uint64_t *result,
                      uint64_t increment);

/*----------------------------------------------------------------------------*/
/* attribute support functions for Nexenta */
alias ulong mdbx_attr_t;

/* Store by cursor with attribute.
 *
 * This function stores key/data pairs into the database. The cursor is
 * positioned at the new item, or on failure usually near it.
 *
 * NOTE: Internally based on MDBX_RESERVE feature,
 *       therefore doesn't support MDBX_DUPSORT.
 *
 * [in] cursor  A cursor handle returned by mdbx_cursor_open()
 * [in] key     The key operated on.
 * [in] data    The data operated on.
 * [in] attr    The attribute.
 * [in] flags   Options for this operation. This parameter must be set to 0
 *              or one of the values described here:
 *
 *	- MDBX_CURRENT
 *      Replace the item at the current cursor position. The key parameter
 *      must still be provided, and must match it, otherwise the function
 *      return MDBX_EKEYMISMATCH.
 *
 *  - MDBX_APPEND
 *      Append the given key/data pair to the end of the database. No key
 *      comparisons are performed. This option allows fast bulk loading when
 *      keys are already known to be in the correct order. Loading unsorted
 *      keys with this flag will cause a MDBX_KEYEXIST error.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_EKEYMISMATCH
 *  - MDBX_MAP_FULL  - the database is full, see mdbx_env_set_mapsize().
 *  - MDBX_TXN_FULL  - the transaction has too many dirty pages.
 *  - MDBX_EACCES    - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_cursor_put_attr(MDBX_cursor *cursor, MDBX_val *key,
                         MDBX_val *data, mdbx_attr_t attr,
                         uint flags);

/* Store items and attributes into a database.
 *
 * This function stores key/data pairs in the database. The default behavior
 * is to enter the new key/data pair, replacing any previously existing key
 * if duplicates are disallowed.
 *
 * NOTE: Internally based on MDBX_RESERVE feature,
 *       therefore doesn't support MDBX_DUPSORT.
 *
 * [in] txn       A transaction handle returned by mdbx_txn_begin().
 * [in] dbi       A database handle returned by mdbx_dbi_open().
 * [in] key       The key to store in the database.
 * [in] attr      The attribute to store in the database.
 * [in,out] data  The data to store.
 * [in] flags     Special options for this operation. This parameter must be
 *                set to 0 or by bitwise OR'ing together one or more of the
 *                values described here:
 *
 *  - MDBX_NOOVERWRITE
 *      Enter the new key/data pair only if the key does not already appear
 *      in the database. The function will return MDBX_KEYEXIST if the key
 *      already appears in the database. The data parameter will be set to
 *      point to the existing item.
 *
 *  - MDBX_CURRENT
 *      Update an single existing entry, but not add new ones. The function
 *      will return MDBX_NOTFOUND if the given key not exist in the database.
 *      Or the MDBX_EMULTIVAL in case duplicates for the given key.
 *
 *  - MDBX_APPEND
 *      Append the given key/data pair to the end of the database. This option
 *      allows fast bulk loading when keys are already known to be in the
 *      correct order. Loading unsorted keys with this flag will cause
 *      a MDBX_EKEYMISMATCH error.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_KEYEXIST
 *  - MDBX_MAP_FULL  - the database is full, see mdbx_env_set_mapsize().
 *  - MDBX_TXN_FULL  - the transaction has too many dirty pages.
 *  - MDBX_EACCES    - an attempt was made to write in a read-only transaction.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_put_attr(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
                  MDBX_val *data, mdbx_attr_t attr, uint flags);

/* Set items attribute from a database.
 *
 * This function stores key/data pairs attribute to the database.
 *
 * NOTE: Internally based on MDBX_RESERVE feature,
 *       therefore doesn't support MDBX_DUPSORT.
 *
 * [in] txn   A transaction handle returned by mdbx_txn_begin().
 * [in] dbi   A database handle returned by mdbx_dbi_open().
 * [in] key   The key to search for in the database.
 * [in] data  The data to be stored or NULL to save previous value.
 * [in] attr  The attribute to be stored.
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *	 - MDBX_NOTFOUND   - the key-value pair was not in the database.
 *	 - MDBX_EINVAL     - an invalid parameter was specified. */
int mdbx_set_attr(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
                  MDBX_val *data, mdbx_attr_t attr);

/* Get items attribute from a database cursor.
 *
 * This function retrieves key/data pairs from the database. The address and
 * length of the key are returned in the object to which key refers (except
 * for the case of the MDBX_SET option, in which the key object is unchanged),
 * and the address and length of the data are returned in the object to which
 * data refers. See mdbx_get() for restrictions on using the output values.
 *
 * [in] cursor    A cursor handle returned by mdbx_cursor_open()
 * [in,out] key   The key for a retrieved item
 * [in,out] data  The data of a retrieved item
 * [in] op        A cursor operation MDBX_cursor_op
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_NOTFOUND  - no matching key found.
 *  - MDBX_EINVAL    - an invalid parameter was specified. */
int mdbx_cursor_get_attr(MDBX_cursor *mc, MDBX_val *key,
                         MDBX_val *data, mdbx_attr_t *attrptr,
                         MDBX_cursor_op op);

/* Get items attribute from a database.
 *
 * This function retrieves key/data pairs from the database. The address
 * and length of the data associated with the specified key are returned
 * in the structure to which data refers.
 * If the database supports duplicate keys (MDBX_DUPSORT) then the
 * first data item for the key will be returned. Retrieval of other
 * items requires the use of mdbx_cursor_get().
 *
 * NOTE: The memory pointed to by the returned values is owned by the
 * database. The caller need not dispose of the memory, and may not
 * modify it in any way. For values returned in a read-only transaction
 * any modification attempts will cause a SIGSEGV.
 *
 * NOTE: Values returned from the database are valid only until a
 * subsequent update operation, or the end of the transaction.
 *
 * [in] txn       A transaction handle returned by mdbx_txn_begin()
 * [in] dbi       A database handle returned by mdbx_dbi_open()
 * [in] key       The key to search for in the database
 * [in,out] data  The data corresponding to the key
 *
 * Returns A non-zero error value on failure and 0 on success, some
 * possible errors are:
 *  - MDBX_NOTFOUND  - the key was not in the database.
 *  - MDBX_EINVAL   - an invalid parameter was specified. */
int mdbx_get_attr(MDBX_txn *txn, MDBX_dbi dbi, MDBX_val *key,
                  MDBX_val *data, mdbx_attr_t *attrptr);
}
