
/**
 * mdbx драйвер
 */
module veda.storage.mdbx.mdbx_driver;

private
{
    import std.stdio, std.file, std.datetime, std.conv, std.digest.ripemd, std.bigint, std.string, std.uuid, core.memory;
    import veda.storage.mdbx.mdbx_header, veda.common.type, veda.common.logger, veda.storage.common;

    alias core.thread.Thread core_thread;
}

public bool[ string ] db_is_open;

/// key-value хранилище на mdbx
public class MdbxDriver : KeyValueDB
{
    MDBX_env            *env;
    public const string summ_hash_this_db_id;
    private BigInt      summ_hash_this_db;
    protected DBMode    mode;
    private string      _path;
    string              db_name;
    string              parent_thread_name;
    private long        last_op_id;
    long                committed_last_op_id;
    Logger              log;
    bool                db_is_opened;
    int                 max_count_record_in_memory = 10_000;

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

        try
        {
            mkdir(_path);
            writeln("create folder: ", path);
        }
        catch (Exception ex)
        {
        }

        open();
//        reopen_db();
    }

    @property
    string path()
    {
        return this._path;
    }

    public long get_last_op_id()
    {
        return last_op_id;
    }

    public void close()
    {
        if (mode == DBMode.RW)
            flush(1);
        mdbx_env_close(env);
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
            log.trace("MDBX:reopen_db %s, mode=%s, thread:%s, last_op_id=%d", _path, text(mode), core_thread.getThis().name, last_op_id);
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

        rc = mdbx_env_create(&env);
        if (rc != 0)
            log.trace_log_and_console("WARN! %s(%s) #1:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdbx_strerror(rc)));
        else
        {
            if (mode == DBMode.RW)
            {
                ulong new_map_size = 10_000_000_000;
                mdbx_env_set_geometry(env, -1, new_map_size, -1, -1, -1, -1);
                uint  mode_flags = MDBX_UTTERLY_NOSYNC | /*MDBX_NORDAHEAD |*/ MDBX_NOMEMINIT | MDBX_COALESCE | MDBX_LIFORECLAIM;

                rc = mdbx_env_open(env, cast(char *)_path, mode_flags, std.conv.octal !664);
            }
            else
                rc = mdbx_env_open(env, cast(char *)_path, MDBX_RDONLY | MDBX_LIFORECLAIM, std.conv.octal !666);


            if (rc != 0)
                log.trace_log_and_console("WARN! %s(%s) #2:%s", __FUNCTION__ ~ ":" ~ text(__LINE__), _path, fromStringz(mdbx_strerror(rc)));
            else
                db_is_open[ _path ] = true;

            if (rc == 0)
            {
                string   data_str = find(OptAuthorize.NO, null, summ_hash_this_db_id);

                string[] dataff = data_str.split(',');
                string   hash_str;
                if (dataff.length == 2)
                {
                    hash_str = dataff[ 0 ];

                    try
                    {
                        last_op_id           = to!long (dataff[ 1 ]);
                        committed_last_op_id = last_op_id;
                    }
                    catch (Throwable tr) {}
                }

                if (hash_str is null || hash_str.length < 1)
                    hash_str = "0";

                summ_hash_this_db = BigInt("0x" ~ hash_str);
                log.trace("MDBX:open MDBX %s data_str=[%s], last_op_id=%d", _path, data_str, last_op_id);
                db_is_opened = true;

                //growth_db(env, null);
            }
        }
    }

    private int growth_db(MDBX_env *env, MDBX_txn *txn)
    {
        int          rc;
        MDBX_envinfo stat;

        if (txn !is null)
            mdbx_txn_abort(txn);

        rc = mdbx_env_info(env, &stat, stat.sizeof);
        if (rc == 0)
        {
            size_t map_size     = stat.mi_mapsize;
            size_t new_map_size = map_size + 1 * map_size;

            log.trace_log_and_console("Growth database (%s) prev MAP_SIZE=" ~ text(map_size) ~ ", new MAP_SIZE=" ~ text(new_map_size),
                                      _path);

            mdbx_txn_begin(env, null, 0, &txn);
//            rc = mdbx_env_set_mapsize(env, new_map_size);
            rc = mdbx_env_set_geometry(env, -1, new_map_size, -1, new_map_size, -1, -1);
            if (rc != MDBX_SUCCESS)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
                core_thread.sleep(dur!("msecs")(100));
                return growth_db(env, txn);
            }
            rc = mdbx_txn_commit(txn);
            if (rc != MDBX_SUCCESS)
            {
                log.trace_log_and_console("rc=%d", rc);
            }

            rc = mdbx_env_sync(env, 1);
            if (rc != MDBX_SUCCESS)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
            }

            core_thread.sleep(dur!("msecs")(10));
        }
        return rc;
    }

    public ResultCode put(OptAuthorize op_auth, string user_uri, string in_key, string in_value, long op_id)
    {
        if (db_is_opened == false)
            open();

        if (op_id > 0)
            last_op_id = op_id;

        try
        {
            string _key  = in_key.dup;
            string value = in_value.dup;

            if (_key is null || _key.length < 1)
                return ResultCode.No_Content;

            if (value is null || value.length < 1)
                return ResultCode.No_Content;

            int      rc;
            MDBX_dbi dbi;
            MDBX_txn *txn;

            rc = mdbx_txn_begin(env, null, 0, &txn);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                mdbx_txn_abort(txn);
                return ResultCode.Fail_Open_Transaction;
            }
            rc = mdbx_dbi_open(txn, null, MDBX_CREATE, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                mdbx_txn_abort(txn);
                return ResultCode.Fail_Open_Transaction;
            }

            MDBX_val key;
            key.iov_base = cast(char *)_key;
            key.iov_len  = _key.length;

            MDBX_val data;
            data.iov_base = cast(char *)value;
            data.iov_len  = value.length;

            rc = mdbx_put(txn, dbi, &key, &data, 0);
            if (rc == MDBX_MAP_FULL)
            {
                log.trace_log_and_console("MAP FULL #1");
                growth_db(env, txn);

                // retry
                return put(op_auth, user_uri, _key, value, op_id);
            }
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                return ResultCode.Fail_Store;
            }

            rc = mdbx_txn_commit(txn);
            if (rc == MDBX_MAP_FULL)
            {
                log.trace_log_and_console("MAP FULL #2");
                growth_db(env, txn);

                // retry
                return put(op_auth, user_uri, _key, value, op_id);
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR! %s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                           rc)),
                                          _key);
                return ResultCode.Fail_Commit;
            }

            mdbx_dbi_close(env, dbi);

            return ResultCode.OK;
        }
        catch (Throwable tr)
        {
            log.trace_log_and_console("ERR!  " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", %s", tr.msg);
            return ResultCode.Fail_Store;
        }
    }

    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        if (db_is_opened == false)
            open();

        try
        {
            string _key = in_key.dup;

            if (_key is null || _key.length < 1)
                return ResultCode.No_Content;

            int      rc;
            MDBX_dbi dbi;
            MDBX_txn *txn;

            rc = mdbx_txn_begin(env, null, 0, &txn);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                mdbx_txn_abort(txn);
                return ResultCode.Fail_Open_Transaction;
            }
            rc = mdbx_dbi_open(txn, null, MDBX_CREATE, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                mdbx_txn_abort(txn);
                return ResultCode.Fail_Open_Transaction;
            }

            MDBX_val key;
            key.iov_base = cast(char *)_key;
            key.iov_len  = _key.length;

            MDBX_val data;
            data.iov_base = null;
            data.iov_len  = 0;

            rc = mdbx_del(txn, dbi, &key, &data);
            if (rc == MDBX_MAP_FULL)
            {
                log.trace_log_and_console("MAP FULL #r1");

                growth_db(env, txn);

                // retry
                return remove(op_auth, user_uri, _key);
            }
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                mdbx_txn_abort(txn);
                return ResultCode.Fail_Store;
            }

            rc = mdbx_txn_commit(txn);
            if (rc == MDBX_MAP_FULL)
            {
                log.trace_log_and_console("MAP FULL #r2");
                growth_db(env, null);

                // retry
                return remove(op_auth, user_uri, _key);
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s, key=%s", _path, fromStringz(mdbx_strerror(
                                                                                                                                          rc)),
                                          _key);
                return ResultCode.Fail_Commit;
            }

            mdbx_dbi_close(env, dbi);

            return ResultCode.OK;
        }
        catch (Throwable tr)
        {
            log.trace_log_and_console("ERR!  " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", %s", tr.msg);
            return ResultCode.Fail_Store;
        }
    }

    public void flush(int force)
    {
        try
        {
            //    log.trace("flush %s last_op_id=%d", _path, last_op_id);
            if (mode == DBMode.RW && last_op_id > committed_last_op_id)
            {
                put(OptAuthorize.NO, null, summ_hash_this_db_id, "0," ~ text(last_op_id), -1);
                committed_last_op_id = last_op_id;
            }

            int rc = mdbx_env_sync(env, force);
            if (rc != MDBX_SUCCESS)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", (%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
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

        MDBX_txn *txn_r;
        MDBX_dbi dbi;

        rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
        if (rc == MDBX_BAD_RSLOT)
        {
            log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
            mdbx_txn_abort(txn_r);

            // TODO: sleep ?
            core_thread.sleep(dur!("msecs")(1));

            rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
        }

        if (rc != 0)
        {
            if (rc == MDBX_MAP_RESIZED)
            {
                log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdbx_strerror(rc)));
                reopen();
                return count_entries();
            }

            log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdbx_strerror(rc)));
            mdbx_txn_abort(txn_r);
            return -1;
        }


        try
        {
            rc = mdbx_dbi_open(txn_r, null, 0, &dbi);
            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
                throw new Exception(cast(string)("Fail:" ~  fromStringz(mdbx_strerror(rc))));
            }

            MDBX_stat stat;
            rc = mdbx_dbi_stat(txn_r, dbi, &stat, stat.sizeof);

            if (rc == 0)
            {
                count = stat.ms_entries;
            }
        }catch (Exception ex)
        {
        }

        mdbx_txn_abort(txn_r);

        return count;
    }

    public string find(OptAuthorize op_auth, string user_uri, string _uri)
    {
        MDBX_txn *txn_r;

        try
        {
            string uri = _uri.idup;

            if (db_is_opened == false)
                open();

            if (uri is null || uri.length < 2)
                return null;

            if (db_is_open.get(_path, false) == false)
                return null;

            string   str = null;
            int      rc;
            MDBX_dbi dbi;

            rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);

            if (rc == MDBX_BAD_RSLOT)
            {
                for (int i = 0; i < 10 && rc != 0; i++)
                {
                    //log.trace_log_and_console("[%s] warn: find:" ~ text(__LINE__) ~ "(%s) MDBX_BAD_RSLOT", parent_thread_name, _path);
                    mdbx_txn_abort(txn_r);

                    // TODO: sleep ?
                    if (i > 3)
                        core_thread.sleep(dur!("msecs")(10));

                    rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
                }
            }

            if (rc != 0)
            {
                if (rc == MDBX_MAP_RESIZED)
                {
                    log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", _path, fromStringz(mdbx_strerror(rc)));
                    reopen();
                    return find(op_auth, user_uri, uri);
                }
                else if (rc == MDBX_BAD_RSLOT)
                {
                    log.trace_log_and_console("WARN! [%s] #2: find:" ~ text(__LINE__) ~ "(%s) MDBX_BAD_RSLOT", parent_thread_name, _path);
                    mdbx_txn_abort(txn_r);

                    // TODO: sleep ?
                    //core.thread.Thread.sleep(dur!("msecs")(1));
                    //rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
                    reopen();
                    rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
                }
            }

            if (rc != 0)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
                return null;
            }

            try
            {
                rc = mdbx_dbi_open(txn_r, null, 0, &dbi);
                if (rc != 0)
                {
                    log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, fromStringz(mdbx_strerror(rc)));
                    return null;
                }

                MDBX_val key;
                key.iov_len  = uri.length;
                key.iov_base = cast(char *)uri;

                MDBX_val data;

                auto     swA = StopWatch();
                swA.start();

                rc = mdbx_get(txn_r, dbi, &key, &data);
                if (rc == 0)
                    str = cast(string)(data.iov_base[ 0..data.iov_len ]).dup;

                swA.stop();
                long tA = cast(long)swA.peek().usecs;

                if (tA > 1000)
                    log.trace("WARN! SLOWLY READ! mdbx.find.mdbx_get %s FINISH %d µs rc=%d", _uri, tA, rc);
            }catch (Exception ex)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", _path, ex.msg);
                return null;
            }

            return str;
        }
        finally
        {
            mdbx_txn_abort(txn_r);
        }
    }
}
