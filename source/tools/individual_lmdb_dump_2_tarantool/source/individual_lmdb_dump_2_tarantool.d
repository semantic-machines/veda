import std.stdio, core.stdc.stdlib;
import std.stdio, std.file, std.datetime, std.conv, std.digest.ripemd, std.bigint, std.string, std.uuid, core.memory;
alias core.thread.Thread core_thread;
import veda.core.common.define;
import veda.storage.lmdb.lmdb_driver, veda.storage.lmdb.lmdb_header;
import veda.storage.tarantool.tarantool_driver, veda.storage.common, veda.common.type, veda.onto.individual;
import veda.util.properd;
import veda.common.logger;

Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("individual-lmdb-dump-2-tarantool", "log", "");
    return _log;
}

void main(string[] args)
{
    if (args.length < 3)
    {
        writefln("need command line argument: [dest db name] [path to lmdb dump file]");
        exit(-1);
        return;
    }

    KeyValueDB individual_tt_storage;
    KeyValueDB ticket_tt_storage;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        individual_tt_storage = new TarantoolDriver(log, "individuals", 512);
        ticket_tt_storage     = new TarantoolDriver(log, "tickets", 513);
    }

    const string individuals_db_path = "./data/lmdb-individuals";
    const string tickets_db_path     = "./data/lmdb-tickets";

    LmdbDriver   individual_lmdb_driver = new LmdbDriver(individuals_db_path, DBMode.R, "cnv", log);



    bool is_prepare = false;

    long counter = 0;

    convert(individual_lmdb_driver, individual_tt_storage);
}


public long convert(LmdbDriver src, KeyValueDB dest)
{
    src.open();

    long    count;

    int     rc;
    MDB_txn *txn_r;
    MDB_dbi dbi;

    rc = mdb_txn_begin(src.env, null, MDB_RDONLY, &txn_r);
    if (rc == MDB_BAD_RSLOT)
    {
        for (int i = 0; i < 10 && rc != 0; i++)
        {
            //log.trace_log_and_console("[%s] warn: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", parent_thread_name, src._path);
            mdb_txn_abort(txn_r);

            // TODO: sleep ?
            if (i > 3)
                core_thread.sleep(dur!("msecs")(10));

            rc = mdb_txn_begin(src.env, null, MDB_RDONLY, &txn_r);
        }
    }

    if (rc != 0)
    {
        if (rc == MDB_MAP_RESIZED)
        {
            log.trace_log_and_console("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", src._path, fromStringz(mdb_strerror(rc)));
            src.reopen();
            return -1;
        }
        else if (rc == MDB_BAD_RSLOT)
        {
            log.trace_log_and_console("WARN! [%s] #2: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", "", src._path);
            mdb_txn_abort(txn_r);

            // TODO: sleep ?
            //core.thread.Thread.sleep(dur!("msecs")(1));
            //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            src.reopen();
            rc = mdb_txn_begin(src.env, null, MDB_RDONLY, &txn_r);
        }
    }

    if (rc != 0)
    {
        log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", src._path, fromStringz(mdb_strerror(rc)));
        return -1;
    }

    try
    {
        rc = mdb_dbi_open(txn_r, null, 0, &dbi);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", src._path, fromStringz(mdb_strerror(rc)));
            return -1;
        }

        MDB_cursor *cursor;

        rc = mdb_cursor_open(txn_r, dbi, &cursor);
        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", src._path, fromStringz(mdb_strerror(rc)));
            return -1;
        }

        MDB_val key;
        MDB_val data;

        while (rc == 0)
        {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_cursor_op.MDB_NEXT);

            if (rc == 0)
            {
                string new_hash;
                string str_key = cast(string)(key.mv_data[ 0..key.mv_size ]).dup;
                if (str_key == xapian_metadata_doc_id || str_key == src.summ_hash_this_db_id || str_key.length == 0)
                    continue;

                string value = cast(string)(data.mv_data[ 0..data.mv_size ]).dup;

                if (value.length == 0)
                    continue;

                Individual indv;
                if (indv.deserialize(value) < 0)
                {
                    writefln("ERR! %d KEY=[%s]", count, key);
                }
                else
                {
                    string new_bin = indv.serialize();
                    dest.put(OptAuthorize.NO, null, str_key, new_bin, -1);
                    writefln("OK, %d KEY=[%s]", count, str_key);
                }

                count++;
            }
        }
    }catch (Exception ex)
    {
        log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", src._path, ex.msg);
        return -1;
    }

    scope (exit)
    {
        mdb_txn_abort(txn_r);
    }

    return count;
}

