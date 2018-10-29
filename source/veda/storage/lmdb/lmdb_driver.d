/**
 * lmdb драйвер
 */

module veda.storage.lmdb.lmdb_driver;

private
{
    import std.stdio, std.file, std.datetime.stopwatch, std.conv, std.digest.ripemd, std.bigint, std.string, std.uuid, core.memory;
    import veda.storage.lmdb.lmdb_header, veda.common.type, veda.common.logger, veda.storage.common, veda.core.common.define;
    import veda.onto.individual;

    alias core.thread.Thread core_thread;
}

public bool[ string ] db_is_open;

/// key-value хранилище на lmdb
public class LmdbDriver : KeyValueDB
{
    MDB_env             *env;
    public const string summ_hash_this_db_id;
    private BigInt      summ_hash_this_db;
    protected DBMode    mode;
    public string       _path;
    string              db_name;
    string              parent_thread_name;
    Logger              log;
    bool                db_is_opened;
    long                read_count;

    /// конструктор
    this(string _path_, DBMode _mode, string _parent_thread_name, Logger _log)
    {
        log                  = _log;
        _path                = _path_;
        db_name              = _path[ (lastIndexOf(path, '/') + 1)..$ ];
        summ_hash_this_db_id = "summ_hash_this_db";
        mode                 = _mode;
        parent_thread_name   = _parent_thread_name;

        string thread_name = core_thread.getThis().name;
        if (thread_name is null || thread_name.length == 0)
        {
            core_thread.getThis().name = "core" ~ text(randomUUID().toHash())[ 0..5 ];
        }

        open();
//        reopen_db();
    }

    @property
    string path()
    {
        return this._path;
    }

    public void close()
    {
        if (mode == DBMode.RW)
            flush(1);
        mdb_env_close(env);
        db_is_open[ _path ] = false;
        GC.collect();

//      writeln ("@@@ close_db, thread:", core.thread.Thread.getThis().name);
    }

    public void reopen()
    {
        if (mode == DBMode.R)
        {
            close();
            open();
            log.trace("reopen_db %s, mode=%s, thread:%s", _path, text(mode), core_thread.getThis().name);
        }
    }

    public void open()
    {
        //log.trace ("@@@ open_db #1 %s, mode=%s, thread:%s",  _path, text(mode), core.thread.Thread.getThis().name);

        if (db_is_open.get(_path, false) == true)
        {
            //log.trace("@@@ open_db #2 ", _path, ", thread:", core.thread.Thread.getThis().name, ", ALREADY OPENNING, db_is_open=", db_is_open);
            return;
        }

        int rc;

        rc = mdb_env_create(&env);
        if (rc != 0)
            log.trace_log_and_console("WARN! %s(%s) #1:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdb_strerror(rc)));
        else
        {
//            rc = mdb_env_open(env, cast(char *)_path, MDB_NOMETASYNC | MDB_NOSYNC | MDB_NOTLS, std.conv.octal !664);

            if (mode == DBMode.RW)
//              rc = mdb_env_open(env, cast(char *)_path, MDB_NOSYNC, std.conv.octal !664);
                rc = mdb_env_open(env, cast(char *)_path, MDB_NOMETASYNC | MDB_NOSYNC, std.conv.octal !664);
            else
                rc = mdb_env_open(env, cast(char *)_path, MDB_RDONLY | MDB_NOMETASYNC | MDB_NOSYNC | MDB_NOLOCK, std.conv.octal !666);


            if (rc != 0)
                log.trace_log_and_console("WARN! %s(%s) #2:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdb_strerror(rc)));
            else
                db_is_open[ _path ] = true;

            read_count = 0;

            if (rc == 0)
            {
                string data_str = get_binobj(summ_hash_this_db_id);

                if (data_str !is null && data_str.length > 0)
                {
                    string[] dataff = data_str.split(',');
                    string   hash_str;
                    if (dataff.length == 2)
                        hash_str = dataff[ 0 ];

                    if (hash_str is null || hash_str.length < 1)
                        hash_str = "0";

                    summ_hash_this_db = BigInt("0x" ~ hash_str);
                }

                log.trace("open LMDB %s data_str=[%s]", _path, data_str);
                db_is_opened = true;
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
                core_thread.sleep(dur!("msecs")(100));
                return growth_db(env, txn);
            }
            core_thread.sleep(dur!("msecs")(10));
        }
        return rc;
    }

    public ResultCode store(string in_key, string in_value, long op_id)
    {
        if (db_is_opened == false)
            open();

        try
        {
            string _key  = in_key.dup;
            string value = in_value.dup;

            if (_key is null || _key.length < 1)
                return ResultCode.NoContent;

            if (value is null || value.length < 1)
                return ResultCode.NoContent;

            int     rc;
            MDB_dbi dbi;
            MDB_txn *txn;

            rc = mdb_txn_begin(env, null, 0, &txn);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                mdb_txn_abort(txn);
                return ResultCode.FailOpenTransaction;
            }
            rc = mdb_dbi_open(txn, null, MDB_CREATE, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                mdb_txn_abort(txn);
                return ResultCode.FailOpenTransaction;
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
                return store(_key, value, op_id);
            }
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                return ResultCode.FailStore;
            }

            rc = mdb_txn_commit(txn);
            if (rc == MDB_MAP_FULL)
            {
                growth_db(env, null);

                // retry
                return store(_key, value, op_id);
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR! %s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                          rc)),
                                          _key);
                return ResultCode.FailCommit;
            }

            mdb_dbi_close(env, dbi);

            return ResultCode.Ok;
        }
        catch (Throwable tr)
        {
            log.trace_log_and_console("ERR!  " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", %s", tr.msg);
            return ResultCode.FailStore;
        }
    }

    public ResultCode remove(string in_key)
    {
        if (db_is_opened == false)
            open();

        try
        {
            string _key = in_key.dup;

            if (_key is null || _key.length < 1)
                return ResultCode.NoContent;

            int     rc;
            MDB_dbi dbi;
            MDB_txn *txn;

            rc = mdb_txn_begin(env, null, 0, &txn);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                mdb_txn_abort(txn);
                return ResultCode.FailOpenTransaction;
            }
            rc = mdb_dbi_open(txn, null, MDB_CREATE, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                mdb_txn_abort(txn);
                return ResultCode.FailOpenTransaction;
            }

            MDB_val key;
            key.mv_data = cast(char *)_key;
            key.mv_size = _key.length;

            MDB_val data;
            data.mv_data = null;
            data.mv_size = 0;

            rc = mdb_del(txn, dbi, &key, &data);
            if (rc == MDB_MAP_FULL)
            {
                growth_db(env, txn);

                // retry
                return remove(_key);
            }
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                mdb_txn_abort(txn);
                return ResultCode.FailStore;
            }

            rc = mdb_txn_commit(txn);
            if (rc == MDB_MAP_FULL)
            {
                growth_db(env, null);

                // retry
                return remove(_key);
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdb_strerror(
                                                                                                                                         rc)),
                                          _key);
                return ResultCode.FailCommit;
            }

            mdb_dbi_close(env, dbi);

            return ResultCode.Ok;
        }
        catch (Throwable tr)
        {
            log.trace_log_and_console("ERR!  " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", %s", tr.msg);
            return ResultCode.FailStore;
        }
    }

    public void flush(int force)
    {
        try
        {
            int rc = mdb_env_sync(env, force);

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            }
        }
        catch (Throwable tr)
        {
            log.trace_log_and_console("ERR!  " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", %s", tr.msg);
        }
    }


    private string get_new_hash(string content)
    {
        ubyte[ 20 ] hash = ripemd160Of(content);
        BigInt msg_hash = "0x" ~ toHexString(hash);
        summ_hash_this_db += msg_hash;
        return toHex(summ_hash_this_db);
    }

    public long count_entries()
    {
        if (db_is_opened == false)
            open();

        long count = -1;
        int  rc;

        if (db_is_open.get(_path, false) == false)
            return -1;

        MDB_txn *txn_r;
        MDB_dbi dbi;

        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdb_strerror(rc)));
            mdb_txn_abort(txn_r);

            // TODO: sleep ?
            core_thread.sleep(dur!("msecs")(1));

            rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdb_strerror(rc)));
                reopen();
                return count_entries();
            }

            log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdb_strerror(rc)));
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


    public void get_individual(string uri, ref Individual individual)
    {
        string individual_as_binobj = get_binobj(uri);

        if (individual_as_binobj is null)
        {
            individual.setStatus(ResultCode.NotFound);
            return;
        }


        if (individual_as_binobj !is null && individual_as_binobj.length > 1)
        {
            if (individual.deserialize(individual_as_binobj) > 0)
                individual.setStatus(ResultCode.Ok);
            else
            {
                individual.setStatus(ResultCode.UnprocessableEntity);
                writeln("ERR!: invalid binobj: [", individual_as_binobj, "] ", uri);
            }
        }
        else
        {
            individual.setStatus(ResultCode.UnprocessableEntity);
            //writeln ("ERR!: empty binobj: [", individual_as_binobj, "] ", uri);
        }
    }

    public string get_binobj(string _uri)
    {
        string uri = _uri.idup;

        if (db_is_opened == false)
            open();

        if (read_count > 100_000)
            reopen();

        if (uri is null || uri.length < 2)
            return null;

        if (db_is_open.get(_path, false) == false)
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
                //log.trace_log_and_console("[%s] warn: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", parent_thread_name, _path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                if (i > 3)
                    core_thread.sleep(dur!("msecs")(10));

                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdb_strerror(rc)));
                reopen();
                return get_binobj(uri);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace_log_and_console("WARN! [%s] #2: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", parent_thread_name, _path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                reopen();
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

            auto    swA = StopWatch();
            swA.start();
            //      log.trace ("lmdb.find.mdb_get START uri=%s", _uri);

            rc = mdb_get(txn_r, dbi, &key, &data);
            if (rc == 0)
                str = cast(string)(data.mv_data[ 0..data.mv_size ]);
            else if (rc == MDB_INVALID)
            {
                log.trace("ERR! MDB_INVALID! lmdb.find, key=%s", uri);
                reopen();
                core_thread.sleep(dur!("msecs")(10));
                return get_binobj(_uri);
            }

            swA.stop();
            long tA = cast(long)swA.peek.total !"msecs";

            if (tA > 50)
                log.trace("WARN! SLOWLY READ! lmdb.find.mdb_get %s FINISH %d ms rc=%d", _uri, tA, rc);

            read_count++;
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
        {
            return str.dup;
        }
        else
            return str;
    }
}
