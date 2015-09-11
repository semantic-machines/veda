/**
 * lmdb реализация хранилища
 */
module storage.lmdb_storage;

private
{
    import std.stdio, std.file, std.datetime, std.conv, std.digest.ripemd, std.bigint, std.string;
    import veda.core.bind.lmdb_header;
    import veda.onto.individual;
    import util.logger, util.utils, util.cbor, veda.core.util.cbor8individual;
    import veda.core.context, veda.core.define;
}

logger _log;
logger log ()
{
	if (_log is null)
    	_log = new logger("pacahon", "log", "lmdb");	
    return _log;	
} 

/// Режим работы хранилища
enum DBMode
{
    /// чтение
    R  = true,

    /// чтение/запись
    RW = false
}

/// Результат
public enum Result
{
    /// OK
    Ok,

    /// Ошибка
    Err,

    /// Ничего
    Nothing
}

bool[ string ] db_is_rw;

/// key-value хранилище на lmdb
public class LmdbStorage
{
    MDB_env             *env;
    public const string summ_hash_this_db_id;
    private BigInt      summ_hash_this_db;
    private DBMode      mode;
    private string      _path;
    string              db_name;
    string              parent_thread_name;

    /// конструктор
    this(string _path_, DBMode _mode, string _parent_thread_name)
    {
        _path                = _path_;
        db_name              = _path[ (lastIndexOf(path, '/') + 1)..$ ];
        summ_hash_this_db_id = "summ_hash_this_db";
        mode                 = _mode;
        parent_thread_name   = _parent_thread_name;

        if (log is null)
            log = new logger("pacahon", "log", "lmdb");

        create_folder_struct();
        open_db();
        reopen_db();
    }

    @property
    string path()
    {
        return this._path;
    }

    public Result backup(string backup_id)
    {
        string backup_path    = dbs_backup ~ "/" ~ backup_id;
        string backup_db_name = dbs_backup ~ "/" ~ backup_id ~ "/" ~ db_name;

        try
        {
            mkdir(backup_path);
        }
        catch (Exception ex)
        {
        }

        try
        {
            mkdir(backup_db_name);
        }
        catch (Exception ex)
        {
        }

        try
        {
            remove(backup_db_name ~ "/" ~ "data.mdb");
        }
        catch (Exception ex)
        {
        }

        flush(1);

        int rc = mdb_env_copy(env, cast(char *)backup_db_name);

        if (rc != 0)
        {
            log.trace_log_and_console("%s(%s) ERR:%s CODE:%d", __FUNCTION__ ~ ":" ~ text(__LINE__), backup_db_name,
                                      fromStringz(mdb_strerror(rc)), rc);
            return Result.Err;
        }

        return Result.Ok;
    }


    public void reopen_db()
    {
        flush(1);

        bool is_cooperative = db_is_rw.get(_path, false);
        //log.trace_log_and_console("%s(%s) INFO: is_cooperative=%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, text (is_cooperative));

        if (is_cooperative == true && mode == DBMode.RW || is_cooperative == false)
        {
            mdb_env_close(env);

            if (mode == DBMode.RW)
                log.trace_log_and_console("%s(%s) INFO: reopen rw db %s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, text(mode));

            open_db();
        }
    }

    public void open_db()
    {
        int rc;

        rc = mdb_env_create(&env);
        if (rc != 0)
            log.trace_log_and_console("%s(%s) WARN#1:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdb_strerror(rc)));
        else
        {
            rc = mdb_env_open(env, cast(char *)_path, MDB_NOMETASYNC | MDB_NOSYNC | MDB_NOTLS, std.conv.octal !664);
            if (rc != 0)
                log.trace_log_and_console("%s(%s) WARN#2:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdb_strerror(rc)));

            if (rc == 0 && mode == DBMode.RW)
            {
                db_is_rw[ _path ] = true;

                string hash_str = find(summ_hash_this_db_id);

                if (hash_str is null || hash_str.length < 1)
                    hash_str = "0";

                summ_hash_this_db = BigInt("0x" ~ hash_str);
                log.trace("%s summ_hash_this_db=%s", _path, hash_str);
            }
        }
    }

    private int growth_db(MDB_env *env, MDB_txn *txn)
    {
        int         rc;
        MDB_envinfo stat;

        if (txn !is null)
            mdb_txn_abort(txn);

        rc = mdb_env_info(env, &stat);
        if (rc == 0)
        {
            size_t map_size     = stat.me_mapsize;
            size_t new_map_size = map_size + 100 * 10_048_576;

            log.trace_log_and_console("Growth database (%s) prev MAP_SIZE=" ~ text(map_size) ~ ", new MAP_SIZE=" ~ text(new_map_size),
                                      _path);

            rc = mdb_env_set_mapsize(env, new_map_size);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
                core.thread.Thread.sleep(dur!("msecs")(100));
                return growth_db(env, txn);
            }
            core.thread.Thread.sleep(dur!("msecs")(10));
        }
        return rc;
    }

    public EVENT update_or_create(string cbor, out string new_hash, EVENT ev = EVENT.NONE)
    {
        // TODO не оптимально!
        Individual ind;

        if (cbor2individual(&ind, cbor) > 0)
            return update_or_create(ind.uri.dup, cbor.dup, new_hash, ev);
        else
            log.trace("!ERR:invalid individual=%s", cbor);
        return EVENT.ERROR;
    }

    public EVENT update_or_create(Individual *ind, out string new_hash, EVENT ev = EVENT.NONE)
    {
        string content = individual2cbor(ind);

        return update_or_create(ind.uri.dup, content.dup, new_hash, ev);
    }

    public ResultCode put(string in_key, string in_value)
    {
		string _key = in_key.dup;
		string value = in_value.dup;
		
        if (_key is null || _key.length < 1)
            return ResultCode.No_Content;

        if (value is null || value.length < 1)
            return ResultCode.No_Content;

        int     rc;
        MDB_dbi dbi;
        MDB_txn *txn;

        rc = mdb_txn_begin(env, null, 0, &txn);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                     rc)),
                                      _key);
            return ResultCode.Fail_Open_Transaction;
        }
        rc = mdb_dbi_open(txn, null, MDB_CREATE, &dbi);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                     rc)),
                                      _key);
            return ResultCode.Fail_Open_Transaction;
        }

        MDB_val key;

        key.mv_data = cast(char *)_key;
        key.mv_size = _key.length;

        MDB_val data;

        data.mv_data = cast(char *)value;
        data.mv_size = value.length;

        rc = mdb_put(txn, dbi, &key, &data, 0);
        if (rc == MDB_MAP_FULL)
        {
            growth_db(env, txn);

            // retry
            return put(_key, value);
        }
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                     rc)),
                                      _key);
            return ResultCode.Fail_Store;
        }

        rc = mdb_txn_commit(txn);
        if (rc == MDB_MAP_FULL)
        {
            growth_db(env, null);

            // retry
            return put(_key, value);
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                     rc)),
                                      _key);
            return ResultCode.Fail_Commit;
        }

        mdb_dbi_close(env, dbi);
        return ResultCode.OK;
    }

    public void flush(int force)
    {
//      writeln ("@FLUSH");
        int rc = mdb_env_sync(env, force);

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
        }
    }


    private string get_new_hash(string content)
    {
        ubyte[ 20 ] hash = ripemd160Of(content);
        BigInt msg_hash = "0x" ~ toHexString(hash);
        summ_hash_this_db += msg_hash;
        return toHex(summ_hash_this_db);
    }



    long count_update;

    private EVENT update_or_create(string uri, string content, out string new_hash, EVENT ev = EVENT.NONE)
    {
//                                      StopWatch sw; sw.start;
        new_hash = get_new_hash(content);

        int     rc;
        MDB_dbi dbi;
        MDB_txn *txn;

        rc = mdb_txn_begin(env, null, 0, &txn);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
        }
        rc = mdb_dbi_open(txn, null, MDB_CREATE, &dbi);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
        }

        MDB_val key;

        key.mv_data = cast(char *)uri;
        key.mv_size = uri.length;

        MDB_val data;

        if (ev == EVENT.NONE)
        {
            // проверим был есть ли такой субьект в базе
            rc = mdb_get(txn, dbi, &key, &data);
            if (rc == 0)
                ev = EVENT.UPDATE;
            else
                ev = EVENT.CREATE;
        }

        data.mv_data = cast(char *)content;
        data.mv_size = content.length;

        rc = mdb_put(txn, dbi, &key, &data, 0);
        if (rc == MDB_MAP_FULL)
        {
            growth_db(env, txn);

            // retry
            return update_or_create(uri, content, new_hash, ev);
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            mdb_txn_abort(txn);
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
        }

        count_update++;

        if (summ_hash_this_db != BigInt.init)
        {   // put current db summ hash
            key.mv_data  = cast(char *)summ_hash_this_db_id;
            key.mv_size  = summ_hash_this_db_id.length;
            data.mv_data = cast(char *)new_hash;
            data.mv_size = new_hash.length;
            rc           = mdb_put(txn, dbi, &key, &data, 0);

            if (rc == MDB_MAP_FULL)
            {
                growth_db(env, txn);

                // retry
                return update_or_create(uri, content, new_hash, ev);
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", put summ_hash (%s) ERR:%s", _path,
                                          fromStringz(mdb_strerror(rc)));
                mdb_txn_abort(txn);
                throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
            }
        }

        rc = mdb_txn_commit(txn);

        if (rc == MDB_MAP_FULL)
        {
            growth_db(env, null);

            // retry
            return update_or_create(uri, content, new_hash, ev);
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            mdb_txn_abort(txn);
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
        }
//                                sw.stop;
//                               long t = sw.peek.usecs;
//                                writeln ("@1 store : t=", t);

        mdb_dbi_close(env, dbi);

        if (count_update % 2_000 == 0)
            reopen_db();

        return ev;
    }

    public long count_entries()
    {
        long    count = -1;
        int     rc;

        MDB_txn *txn_r;
        MDB_dbi dbi;

        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace_log_and_console("warn:" ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            mdb_txn_abort(txn_r);

            // TODO: sleep ?
            core.thread.Thread.sleep(dur!("msecs")(1));

            rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) WARN:%s", _path, fromStringz(mdb_strerror(rc)));
                mdb_env_close(env);
                open_db();

                return count_entries();
            }

            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            mdb_txn_abort(txn_r);
            return -1;
        }


        try
        {
            rc = mdb_dbi_open(txn_r, null, 0, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
                throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));
            }

            MDB_stat stat;
            rc = mdb_stat(txn_r, dbi, &stat);

            if (rc == 0)
            {
                count = stat.ms_entries;
            }
        }catch (Exception ex)
        {
        }

        mdb_txn_abort(txn_r);

        return count;
    }

    public string find(string uri)
    {
        if (uri is null || uri.length < 2)
            return null;

        string  str = null;
        int     rc;
        MDB_txn *txn_r;
        MDB_dbi dbi;

        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                log.trace_log_and_console("[%s] warn: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", parent_thread_name, _path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                if (i > 3)
                    core.thread.Thread.sleep(dur!("msecs")(10));

                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) WARN:%s", _path, fromStringz(mdb_strerror(rc)));
                mdb_env_close(env);
                open_db();

                return find(uri);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace_log_and_console("[%s] warn 2: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", parent_thread_name, _path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                mdb_env_close(env);
                open_db();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            return null;
        }

        try
        {
            rc = mdb_dbi_open(txn_r, null, 0, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
                return null;
            }

            MDB_val key;
            key.mv_size = uri.length;
            key.mv_data = cast(char *)uri;

            MDB_val data;
            rc = mdb_get(txn_r, dbi, &key, &data);
            if (rc == 0)
            {
                str = cast(string)(data.mv_data[ 0..data.mv_size ]);
            }
        }catch (Exception ex)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, ex.msg);
            return null;
        }

        scope (exit)
        {
            mdb_txn_abort(txn_r);
        }

        if (str !is null)
            return str.dup;
        else
            return str;
    }

    public Individual find_individual(string uri)
    {
        Individual ind;
        string     str = find(uri);

        if (str !is null)
        {
            if (cbor2individual(&ind, str) < 0)
            {
                log.trace("!ERR:invalid individual=", uri);
            }
        }
        return ind;
    }
}

string get_new_binlog_name(string db_path)
{
    string now = Clock.currTime().toISOExtString();

    now = now[ 0..indexOf(now, '.') + 4 ];

    return db_path ~ "." ~ now;
}


