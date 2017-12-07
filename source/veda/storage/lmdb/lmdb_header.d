/**
 * обвязка к lmdb
 */
module veda.storage.lmdb.lmdb_header;

import core.sys.posix.sys.stat;

nothrow extern (C) {
version (Posix) {
    alias mode_t mdb_mode_t;
    alias int    mdb_filehandle_t;
}

version (Windows) {
    alias int  mdb_mode_t;
    alias void *mdb_filehandle_t;
}

alias void MDB_env;
alias void MDB_txn;
alias uint MDB_dbi;
alias void MDB_cursor;

/**

   Generic structure used for passing keys and data in and out of the database.
   Values returned from the database are valid only until a subsequent update operation, or the end of the transaction. Do not modify or free them, they commonly point into the database itself.
   Key sizes must be between 1 and mdb_env_get_maxkeysize() inclusive. The same applies to data sizes in databases with the MDB_DUPSORT flag. Other data items can in theory be from 0 to 0xffffffff bytes long.

 */
struct MDB_val
{
    /// size of the data item
    size_t mv_size;

    /// address of the data item
    void   *mv_data;
}

///-
alias int MDB_cmp_func(const MDB_val *a, const MDB_val *b);

///-
alias void MDB_rel_func(MDB_val *item, void *oldptr, void *newptr, void *relctx);

/// mdb_env
enum
{
    MDB_FIXEDMAP   =    0x01,     ///-
    MDB_NOSUBDIR   =    0x4000,   ///-
    MDB_NOSYNC     =    0x10000,  ///-
    MDB_RDONLY     =    0x20000,  ///-
    MDB_NOMETASYNC =    0x40000,  ///-
    MDB_WRITEMAP   =    0x80000,  ///-
    MDB_MAPASYNC   =    0x100000, ///-
    MDB_NOTLS      =    0x200000, ///-
    MDB_NOLOCK     =    0x400000  ///-
}

/*

   MDB_FIXEDMAP использовать фиксированный адрес для области mmap. Этот флаг должен быть указан при создании среды (окружения),
   и хранится постоянно в окружающей среде. В случае успеха, карта памяти всегда будет находиться по тому же виртуальному адресу и
   указатели, используемые для ссылки на данные в базе данных будут постоянными и сохранятся между вызовами.
   Эта опция не всегда может работать, в зависимости от того, как операционная система выделяет память для разделяемых библиотек и
   других целей. Функция является экспериментальной.

   MDB_NOSUBDIR По умолчанию, LMDB создает свою среду в каталог, имя которого задается в пути, и создает свои файлы данных и
   файлы блокировки в этом каталоге. С помощью этой опции определяется путь для основного файла данных. Файл блокировки базы данных -
   это путь с добавленным суффиксом "-lock".

   MDB_RDONLY Открытие среды в режиме только для чтения. Операции записи не допускаются. LMDB будет по-прежнему изменять файл блокировки,
   за исключением файловых систем "только для чтения", где LMDB не использует блокировки.

   MDB_WRITEMAP Использовать записываемую карту памяти, если флаг не установлен MDB_RDONLY. Это быстрее и и вызывает меньше операций
   выделения памяти, но теряется защита от ошибок приложений (например, запись по неверному указателю, и других "плохих" операций записи
   в базу данных). Флаг несовместим с вложенными транзакциями. Не смешивайте процессы с и без MDB_WRITEMAP в одной и той же среде.
   Это может нарушить стабильность (устойчивость) работы  (mdb_env_sync и т.д.).

   MDB_NOMETASYNC Сбрасывать системные буферы на диск только один раз за транзакцию, пропустить сброс метаданных на диск.
   Отложить это до тех пор пока система не сбросит файлы на диск, или до следующего коммита (не MDB_RDONLY) или до вызова mdb_env_sync().
   Эта оптимизация обеспечивает целостность данных, но сбой системы может отменить последнюю совершенную транзакцию.
   Т.е. он сохраняет свойства ACI (атомарность, согласованность, изоляция), но не D (долговечность).
   Этот флаг может быть изменен в любое время с помощью mdb_env_set_flags ().

   MDB_NOSYNC Не сбрасывать системные буферы на диск при совершении транзакции. Эта оптимизация означает, что крах системы может привести
   к повреждению базы данных или потери последних транзакций, если буферы не были сброшены на диск.
   Риск определяется тем, как часто система сбрасывает грязные буферы на диск и как часто вызывается mdb_env_sync ().
   Однако, если файловая система сохраняет порядок записи и MDB_WRITEMAP флаг не используется, транзакции сохраняют
   свойства ACI (атомарность, согласованность, изолированность), но теряют свойство D (прочность). То есть, целостность базы данных
   обеспечивается, но сбой системы может отменить последние транзакции. Обратите внимание, что (MDB_NOSYNC | MDB_WRITEMAP)
   делает невозможным для системы определить, когда нужно записать транзакции на диск до тех пор пока не будет вызван mdb_env_sync ().
   (MDB_MAPASYNC | MDB_WRITEMAP) может быть предпочтительнее. Этот флаг может быть изменен в любое время с помощью mdb_env_set_flags ().

   MDB_MAPASYNC при использовании MDB_WRITEMAP, осуществлять сброс данных на диск асинхронно. Как и при использовании MDB_NOSYNC, сбой
   системы может привести к повреждению базы данных или потере последних транзакций. Вызов mdb_env_sync () обеспечивает целостность
   базы данных на диске до следующего сброса данных на диск. Этот флаг может быть изменен в любое время с помощью mdb_env_set_flags ().

   MDB_NOTLS Не использовать хранилище локализованное в потоке. Привязать слоты таблицы блокировки читателя к объектам MDB_txn, а не к
   потокам. Т.е. mdb_txn_reset () сохраняет слот зарезервированный для объекта MDB_txn. Поток может использовать параллельные транзакции
   чтения. Транзакция чтения может распростаняться на несколько потоков, если пользователь обеспечивает её (их?) синхронизацию.
   Приложения, которые мультиплексируют множество пользовательских потоков над отдельными потоками ОС, должны использовать этот флаг.
   Такое приложение также должно сериализовывать транзакции записи в потоке ОС, так как механизм блокироки при записи в LMDB ничего
   не знает о пользовательских потоках.

   MDB_NOLOCK Не делать никаких блокировок. Если ожидается одновременный доступ, вызывающий сам должен этим управлять.
   Для правильной работы вызывающий должен обеспечить схему "один писатель", и должен обеспечить, чтобы читатели не использовали
   старые транзакции в то время, как писатель активен. Самый простой подход заключается в использовании эксклюзивной блокировки,
   при которой блокируется чтение если начинается транзакция записи.

   MDB_NORDAHEAD Отключить упреждающее чтение. Большинство операционных систем по-умолчанияю выполняют упреждающее чтение при запросах
   на чтение. Этот флаг отключает его, если ОС это поддерживает. При этом может повыситься производительность произвольного чтения,
   когда БД больше оперативной памяти и свободная оперативная память отсутствует. Эта возможность отсутствует в Windows.

   MDB_NOMEMINIT Не инициализировать выделенную память, прежде чем писать в неиспользованное пространство в файле данных.
   По умолчанию, память для страниц  записываемых в файл данных выделяется при использовании malloc. В то время как эти страницы могут
   быть повторно использованы в последующих операциях, эти страницы инициализируются нулями перед использованием. Это позволяет избежать
   записи данных оставшихся от другого кода (который использовал кучу, а затем освободил память) в файл данных. Обратите внимание,
   что многие другие системные библиотеки могут выделять и освобождать память из кучи для собственных нужд. Например, stdio может
   использовать кучу для буферов ввода-вывода файлов. Этот шаг инициализации отнимает немного производительности, поэтому некоторые
   приложения могут отключить его с помощью этого флага. Эта опция может быть проблемой для приложений, которые обрабатывают
   конфиденциальные данные, такие как пароли, и вызывает бурную реакцию у утилит проверки памяти, например Valgrind. Этот флаг не
   используется с флагом MDB_WRITEMAP, при котором запись осуществляется непосредственно в mmap вместо использования malloc для выделения
   страниц. Инициализация также пропускается, если используется флаг MDB_RESERVE. От вызывающего ожидается перезапись всей памяти,
   которая была зарезервирована. Этот флаг может быть изменен в любое время с помощью mdb_env_set_flags ().

 */


/// mdb_dbi_open
enum
{
    MDB_REVERSEKEY =    0x02,   ///-
    MDB_DUPSORT    =    0x04,   ///-
    MDB_INTEGERKEY =    0x08,   ///-
    MDB_DUPFIXED   =    0x10,   ///-
    MDB_INTEGERDUP =    0x20,   ///-
    MDB_REVERSEDUP =    0x40,   ///-
    MDB_CREATE     =    0x40000 ///-
}

/// mdb_put
enum
{
    MDB_NOOVERWRITE =   0x10,    ///-
    MDB_NODUPDATA   =   0x20,    ///-
    MDB_CURRENT     =   0x40,    ///-
    MDB_RESERVE     =   0x10000, ///-
    MDB_APPEND      =   0x20000, ///-
    MDB_APPENDDUP   =   0x40000, ///-
    MDB_MULTIPLE    =   0x80000  ///-
}

/**
   Cursor Get operations.

   This is the set of all operations for retrieving data using a cursor.
 */
enum MDB_cursor_op
{
    MDB_FIRST,          /// Position at first key/data item
    MDB_FIRST_DUP,      /// Position at first data item of current key. Only for #MDB_DUPSORT
    MDB_GET_BOTH,       /// Position at key/data pair. Only for #MDB_DUPSORT
    MDB_GET_BOTH_RANGE, /// position at key, nearest data. Only for #MDB_DUPSORT
    MDB_GET_CURRENT,    /// Return key/data at current cursor position
    MDB_GET_MULTIPLE,   /// Return all the duplicate data items at the current cursor position. Only for #MDB_DUPFIXED
    MDB_LAST,           /// Position at last key/data item
    MDB_LAST_DUP,       /// Position at last data item of current key. Only for #MDB_DUPSORT
    MDB_NEXT,           /// Position at next data item
    MDB_NEXT_DUP,       /// Position at next data item of current key. Only for #MDB_DUPSORT
    MDB_NEXT_MULTIPLE,  /// Return all duplicate data items at the next cursor position. Only for #MDB_DUPFIXED
    MDB_NEXT_NODUP,     /// Position at first data item of next key
    MDB_PREV,           /// Position at previous data item
    MDB_PREV_DUP,       /// Position at previous data item of current key. Only for #MDB_DUPSORT
    MDB_PREV_NODUP,     /// Position at last data item of previous key
    MDB_SET,            /// Position at specified key
    MDB_SET_KEY,        /// Position at specified key, return key + data
    MDB_SET_RANGE       /// Position at first key greater than or equal to specified key.
}

/// errors
enum
{
    /// success
    MDB_SUCCESS          =  0,
    /// key/data pair already exists
    MDB_KEYEXIST         = -30799,
    /// key/data pair not found (EOF)
    MDB_NOTFOUND         = -30798,
    /// Requested page not found - this usually indicates corruption
    MDB_PAGE_NOTFOUND    = -30797,
    /// Located page was wrong type
    MDB_CORRUPTED        =  -30796,
    /// Update of meta page failed, probably I/O error
    MDB_PANIC            = -30795,
    /// Environment version mismatch
    MDB_VERSION_MISMATCH = -30794,
    /// File is not a valid MDB file
    MDB_INVALID          = -30793,
    /// Environment mapsize reached
    MDB_MAP_FULL         = -30792,
    /// Environment maxdbs reached
    MDB_DBS_FULL         = -30791,
    /// Environment maxreaders reached
    MDB_READERS_FULL     = -30790,
    /// Too many TLS keys in use - Windows only
    MDB_TLS_FULL         = -30789,
    /// Txn has too many dirty pages
    MDB_TXN_FULL         = -30788,
    /// Cursor stack too deep - internal error
    MDB_CURSOR_FULL      = -30787,
    /// Page has not enough space - internal error
    MDB_PAGE_FULL        = -30786,
    /// Database contents grew beyond environment mapsize
    MDB_MAP_RESIZED      = -30785,
    /// MDB_INCOMPATIBLE: Operation and DB incompatible, or DB flags changed
    MDB_INCOMPATIBLE     = -30784,
    /// Invalid reuse of reader locktable slot
    MDB_BAD_RSLOT        = -30783,
    /// Transaction cannot recover - it must be aborted
    MDB_BAD_TXN          = -30782,
    /// Too big key/data, key is empty, or wrong DUPFIXED size
    MDB_BAD_VALSIZE      = -30781,
    /// last err code
    MDB_LAST_ERRCODE     = MDB_BAD_VALSIZE
}

/// statistics for a database in the environment
struct MDB_stat
{
    uint   ms_psize;            /// Size of a database page. This is currently the same for all databases.
    uint   ms_depth;            /// Depth (height) of the B-tree
    size_t ms_branch_pages;     /// Number of internal (non-leaf) pages
    size_t ms_leaf_pages;       /// Number of leaf pages
    size_t ms_overflow_pages;   /// Number of overflow pages
    size_t ms_entries;          /// Number of data items
}

/// information about the environment
struct MDB_envinfo
{
    void   *me_mapaddr;         /// Address of map, if fixed
    size_t me_mapsize;          /// Size of the data memory map
    size_t me_last_pgno;        /// ID of the last used page
    size_t me_last_txnid;       /// ID of the last committed transaction
    uint   me_maxreaders;       /// max reader slots in the environment
    uint   me_numreaders;       /// max reader slots used in the environment
}

///-
char *mdb_version(int *major, int *minor, int *patch);

///-
char *mdb_strerror(int err);

///-
int mdb_env_create(MDB_env **env);

///-
int mdb_env_open(MDB_env *env, const char *path, uint flags, mdb_mode_t mode);

///-
int mdb_env_copy(MDB_env *env, const char *path);

///-
int mdb_env_copyfd(MDB_env *env, mdb_filehandle_t fd);

///-
int mdb_env_stat(MDB_env *env, MDB_stat *stat);

///-
int mdb_env_info(MDB_env *env, MDB_envinfo *stat);

///-
int mdb_env_sync(MDB_env *env, int force);

///-
void mdb_env_close(MDB_env *env);

///-
int mdb_env_set_flags(MDB_env *env, uint flags, int onoff);

///-
int mdb_env_get_flags(MDB_env *env, uint *flags);

///-
int mdb_env_get_path(MDB_env *env, const char **path);

///-
int mdb_env_set_mapsize(MDB_env *env, size_t size);

///-
int mdb_env_set_maxreaders(MDB_env *env, uint readers);

///-
int mdb_env_get_maxreaders(MDB_env *env, uint *readers);

///-
int mdb_env_set_maxdbs(MDB_env *env, MDB_dbi dbs);

///-
int mdb_env_get_maxkeysize(MDB_env *env);

///-
int mdb_txn_begin(MDB_env *env, MDB_txn *parent, uint flags, MDB_txn **txn);

///-
MDB_env *mdb_txn_env(MDB_txn *txn);

///-
int mdb_txn_commit(MDB_txn *txn);

///-
void mdb_txn_abort(MDB_txn *txn);

///-
void mdb_txn_reset(MDB_txn *txn);

///-
int mdb_txn_renew(MDB_txn *txn);

///-
int mdb_dbi_open(MDB_txn *txn, const char *name, uint flags, MDB_dbi *dbi);

///-
int mdb_stat(MDB_txn *txn, MDB_dbi dbi, MDB_stat *stat);

///-
int mdb_dbi_flags(MDB_txn *txn, MDB_dbi dbi, uint *flags);

///-
void mdb_dbi_close(MDB_env *env, MDB_dbi dbi);

///-
int mdb_drop(MDB_txn *txn, MDB_dbi dbi, int del);

///-
int mdb_set_compare(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp);

///-
int mdb_set_dupsort(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp);

///-
int mdb_set_relfunc(MDB_txn *txn, MDB_dbi dbi, MDB_rel_func *rel);

///-
int mdb_set_relctx(MDB_txn *txn, MDB_dbi dbi, void *ctx);

///-
int mdb_get(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data);

///-
int mdb_put(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data, uint flags);

///-
int mdb_del(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data);

///-
int mdb_cursor_open(MDB_txn *txn, MDB_dbi dbi, MDB_cursor **cursor);

///-
void mdb_cursor_close(MDB_cursor *cursor);

///-
int mdb_cursor_renew(MDB_txn *txn, MDB_cursor *cursor);

///-
MDB_txn *mdb_cursor_txn(MDB_cursor *cursor);

///-
MDB_dbi mdb_cursor_dbi(MDB_cursor *cursor);

///-
int mdb_cursor_get(MDB_cursor *cursor, MDB_val *key, MDB_val *data, MDB_cursor_op op);

///-
int mdb_cursor_put(MDB_cursor *cursor, MDB_val *key, MDB_val *data, uint flags);

///-
int mdb_cursor_del(MDB_cursor *cursor, uint flags);

///-
int mdb_cursor_count(MDB_cursor *cursor, size_t *countp);

///-
int mdb_cmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b);

///-
int mdb_dcmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b);

///-
alias int MDB_msg_func(const char *msg, void *ctx);

///-
int mdb_reader_list(MDB_env *env, MDB_msg_func *func, void *ctx);

///-
int mdb_reader_check(MDB_env *env, int *dead);
}

