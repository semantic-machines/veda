module veda.storage.lmdb.lmdb_acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.common.type, veda.onto.individual, veda.onto.resource, veda.storage.lmdb.lmdb_header, veda.core.common.context;
    import veda.core.common.define, veda.core.common.know_predicates, veda.core.common.log_msg, veda.common.type;
    import veda.core.util.utils, veda.common.logger;
    import veda.storage.lmdb.lmdb_driver, veda.storage.right_set, veda.storage.common;
    import veda.util.container, veda.util.module_info, veda.storage.authorization;
}

const string acl_indexes_db_path = "./data/acl-indexes";

static this()
{
    paths_list ~= acl_indexes_db_path;
}

/// Хранение, чтение PermissionStatement, Membership
class LmdbAuthorization : Authorization
{
    Logger     log;
    LmdbDriver lmdb_driver;

    // short life time vars
    MDB_val key;
    MDB_val data;
    MDB_txn *txn_r;
    MDB_dbi dbi;

    this(DBMode mode, string _parent_thread_name, Logger _log)
    {
        log         = _log;
        lmdb_driver = new LmdbDriver(acl_indexes_db_path, mode, _parent_thread_name, log);
    }



////////////////////////////////// DB interaction part

    override void reopen()
    {
        lmdb_driver.reopen();
    }

    override bool open()
    {
        if (lmdb_driver.db_is_opened == false)
            lmdb_driver.open();

        return lmdb_driver.db_is_opened;
    }

    override void close()
    {
        lmdb_driver.close();
    }

//    override void flush(int force)
//    {
//        lmdb_driver.flush(force);
//    }

    override string get_in_current_transaction(string in_key)
    {
        string sres;

        key.mv_size = in_key.length;
        key.mv_data = cast(char *)in_key;
        rc          = mdb_get(txn_r, dbi, &key, &data);
        if (rc == 0)
            sres = cast(string)(data.mv_data[ 0..data.mv_size ]).dup;

        return sres;
    }

    override void abort_transaction()
    {
        if (txn_r != null)
            mdb_txn_abort(txn_r);
    }

    override bool begin_transaction(bool is_check_for_reload)
    {
        if (is_check_for_reload)
            acl_check_for_reload(&reopen);

        rc = mdb_txn_begin(lmdb_driver.env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", lmdb_driver.path);
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                mdb_txn_abort(txn_r);

                if (i > 3)
                {
                    log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", lmdb_driver.path);
                    core.thread.Thread.sleep(dur!("msecs")(10));
                }

                rc = mdb_txn_begin(lmdb_driver.env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) %s", lmdb_driver.path, fromStringz(mdb_strerror(rc)));
                reopen();
                rc = mdb_txn_begin(lmdb_driver.env, null, MDB_RDONLY, &txn_r);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace("WARN! 2: find:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", lmdb_driver.path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                reopen();
                rc = mdb_txn_begin(lmdb_driver.env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) ERR! %s", lmdb_driver.path, fromStringz(mdb_strerror(rc)));
            return false;
        }

        rc = mdb_dbi_open(txn_r, null, MDB_CREATE, &dbi);
        if (rc != 0)
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));

        return true;
    }
}
///////////////////////////

private ModuleInfoFile[ MODULE ] info_r__2__pmodule;
private MInfo get_info(MODULE module_id)
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

int  _timeout                         = 10;
long last_committed_op_id_acl_manager = 0;
public bool acl_check_for_reload(void delegate() load)
{
    MInfo mi = get_info(MODULE.acl_preparer);

    //log.trace ("acl_check_for_reload #1, last_committed_op_id_acl_manager=%d, mi=%s", last_committed_op_id_acl_manager, mi);
    if (last_committed_op_id_acl_manager < mi.committed_op_id)
    {
        last_committed_op_id_acl_manager = mi.committed_op_id;
        //log.trace ("acl_check_for_reload #2, last_committed_op_id_acl_manager=%d", last_committed_op_id_acl_manager);
        return true;
    }
    return false;
}
