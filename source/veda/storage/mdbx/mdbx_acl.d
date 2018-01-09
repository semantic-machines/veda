module veda.storage.mdbx.mdbx_acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string;
    import veda.common.type, veda.core.common.define, veda.common.logger, veda.util.module_info;
    import veda.authorization.right_set, veda.storage.common, veda.authorization.authorization;
    import veda.storage.mdbx.mdbx_header, veda.storage.mdbx.mdbx_driver;
}

/// Хранение, чтение PermissionStatement, Membership
class MdbxAuthorization : ImplAuthorization
{
    MdbxDriver driver;

    // short life time vars
    MDBX_val key;
    MDBX_val data;
    MDBX_txn *txn_r;
    MDBX_dbi dbi;

    this(DBMode mode, string _parent_thread_name, Logger _log)
    {
        log    = _log;
        driver = new MdbxDriver(acl_indexes_db_path, mode, _parent_thread_name, log);
    }

    bool open()
    {
        if (driver.db_is_opened == false)
            driver.open();

        return driver.db_is_opened;
    }

    void reopen()
    {
        driver.reopen();
    }

    void close()
    {
        driver.close();
    }

    override string get_in_current_transaction(string in_key)
    {
        string sres;

        key.iov_len  = in_key.length;
        key.iov_base = cast(char *)in_key;
        rc           = mdbx_get(txn_r, dbi, &key, &data);
        if (rc == 0)
            sres = cast(string)(data.iov_base[ 0..data.iov_len ]).dup;

        return sres;
    }

    override void abort_transaction()
    {
        if (txn_r != null)
        {
            mdbx_dbi_close(driver.env, dbi);
            mdbx_txn_abort(txn_r);
        }
    }

    override bool begin_transaction(bool is_check_for_reload)
    {
        if (is_check_for_reload)
            acl_check_for_reload(&reopen, log);

        rc = mdbx_txn_begin(driver.env, null, MDBX_RDONLY, &txn_r);
        if (rc == MDBX_BAD_RSLOT)
        {
            log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDBX_BAD_RSLOT", driver.path);
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                mdbx_txn_abort(txn_r);

                if (i > 3)
                {
                    log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDBX_BAD_RSLOT", driver.path);
                    core.thread.Thread.sleep(dur!("msecs")(10));
                }

                rc = mdbx_txn_begin(driver.env, null, MDBX_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDBX_MAP_RESIZED)
            {
                log.trace("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) %s", driver.path, fromStringz(mdbx_strerror(rc)));
                reopen();
                rc = mdbx_txn_begin(driver.env, null, MDBX_RDONLY, &txn_r);
            }
            else if (rc == MDBX_BAD_RSLOT)
            {
                log.trace("WARN! 2: find:" ~ text(__LINE__) ~ ",%s) MDBX_BAD_RSLOT", driver.path);
                mdbx_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdbx_txn_begin(env, null, MDBX_RDONLY, &txn_r);
                reopen();
                rc = mdbx_txn_begin(driver.env, null, MDBX_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) ERR! %s", driver.path, fromStringz(mdbx_strerror(rc)));
            return false;
        }

        rc = mdbx_dbi_open(txn_r, null, MDBX_CREATE, &dbi);
        if (rc != 0)
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdbx_strerror(rc))));

        return true;
    }
}
///////////////////////////

private ModuleInfoFile[ MODULE ] info_r__2__pmodule;
private MInfo get_info(MODULE module_id, Logger log)
{
    ModuleInfoFile mdif = info_r__2__pmodule.get(module_id, null);

    if (mdif is null)
    {
        mdif                            = new ModuleInfoFile(text(module_id), log, OPEN_MODE.READER);
        info_r__2__pmodule[ module_id ] = mdif;
    }
    MInfo info = mdif.get_info();
    return info;
}

private int  _timeout                         = 10;
private long last_committed_op_id_acl_manager = 0;
private bool acl_check_for_reload(void delegate() load, Logger log)
{
    MInfo mi = get_info(MODULE.acl_preparer, log);

    //log.trace ("acl_check_for_reload #1, last_committed_op_id_acl_manager=%d, mi=%s", last_committed_op_id_acl_manager, mi);
    if (last_committed_op_id_acl_manager < mi.committed_op_id)
    {
        last_committed_op_id_acl_manager = mi.committed_op_id;
        //log.trace ("acl_check_for_reload #2, last_committed_op_id_acl_manager=%d", last_committed_op_id_acl_manager);
        return true;
    }
    return false;
}
