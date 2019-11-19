/**
 * utils
 */

module veda.core.util.utils;

private
{
    import core.stdc.string, core.sys.posix.time;
    import std.file, std.datetime, std.json, std.format, std.stdio, std.conv, std.string, std.concurrency, std.digest.crc;
    import std.ascii, std.csv, std.typecons, std.outbuffer;
    import veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.core.common.type;
    import veda.common.type;
}

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log(){
    if (_log is null)
        _log = new Logger("veda-core-" ~ process_name, "log", "UTIL");
    return _log;
}

int get_slot(ref int[ string ] key2slot, string key, Logger log_if_err = null){
    if (key.length < 1) {
        if (log_if_err !is null)
            log_if_err.trace("ERR! key2slot, key is empty");
        return -1;
    }

    if (key[ 0 ] == '#') {
        try
        {
            int slot = to!int (key[ 1..$ ]);
            return slot;
        }
        catch (Throwable tr)
        {
            if (log_if_err !is null)
                log_if_err.trace("ERR! key2slot, slot not found, invalid key=%s", key);

            return -1;
        }
    }

    int slot = key2slot.get(key, -1);

    if (slot < 0) {
        if (log_if_err !is null)
            log_if_err.trace("ERR! key2slot, slot not found, key=%s", key);
    }

    return slot;
}

public void subject2Ticket(ref Individual ticket, Ticket *tt){
    string when;
    long   duration;

    tt.id         = ticket.uri;
    tt.user_login = ticket.getFirstLiteral("ticket:login");
    tt.user_uri   = ticket.getFirstLiteral("ticket:accessor");
    when          = ticket.getFirstLiteral("ticket:when");
    string dd = ticket.getFirstLiteral("ticket:duration");

    try
    {
        duration = parse!uint (dd);
    }
    catch (Exception ex)
    {
        writeln("Ex!: ", __FUNCTION__, ":", text(__LINE__), ", ", ex.msg);
    }

    if (tt.user_uri is null) {
        log.trace("found a session ticket is not complete, the user can not be found.");
    }

    if (tt.user_uri !is null && (when is null || duration < 10)) {
        log.trace("found a session ticket is not complete, we believe that the user has not been found.");
        tt.user_uri = null;
    }

    if (when !is null) {
        long start_time = stringToTime(when);

        tt.start_time = start_time;
        tt.end_time   = start_time + duration * 10_000_000;
    }
}

// ////// ////// ///////////////////////////////////////////
public Individual *indv_apply_cmd(INDV_OP cmd, Individual *prev_indv, Individual *indv){
    if (prev_indv !is null) {
        if (prev_indv.resources.get("rdf:type", Resources.init).length == 0) {
            log.trace("WARN! stores individual does not contain any type: arg:[%s] prev_indv:[%s]", text(*indv), text(*prev_indv));
        }

        foreach (predicate; indv.resources.keys) {
            if (cmd == INDV_OP.ADD_IN) {
                // add value to set or ignore if exists
                prev_indv.addUniqueResources(predicate, indv.getResources(predicate));
            }else if (cmd == INDV_OP.SET_IN) {
                // set value to predicate
                prev_indv.setResources(predicate, indv.getResources(predicate));
            }else if (cmd == INDV_OP.REMOVE_FROM) {
                // remove predicate or value in set
                prev_indv.removeResources(predicate, indv.getResources(predicate));
            }
        }
    }
    return prev_indv;
}

private Tid[ P_MODULE ] name_2_tids;

public Tid getTid(P_MODULE tid_id){
    if (name_2_tids.values.length == 0) {
        foreach (id; P_MODULE.min .. P_MODULE.max) {
            name_2_tids[ id ] = locate(text(id));
        }
    }

    Tid res = name_2_tids.get(tid_id, Tid.init);

    if (res == Tid.init) {
        // tid not found, attempt restore
        Tid tmp_tid = locate(text(tid_id));

        if (tmp_tid == Tid.init) {
//                writeln("!!! NOT FOUND TID=", text(tid_id), "\n", name_2_tids, ", locate=1 ", );
            //throw new Exception("!!! NOT FOUND TID=" ~ text(tid_id));
            log.trace("!!! NOT FOUND TID=%s", text(tid_id));
            return tmp_tid;
        }else  {
            name_2_tids[ tid_id ] = tmp_tid;
            return tmp_tid;
        }
    }
    return res;
}



bool wait_starting_module(P_MODULE tid_idx, Tid tid){
    bool res;

    if (tid == Tid.init)
        throw new Exception("wait_starting_thread: Tid=" ~ text(tid_idx) ~ " not found", __FILE__, __LINE__);

    log.trace("START THREAD... : %s", text(tid_idx));
    send(tid, thisTid);
    receive((bool isReady)
            {
                res = isReady;
                if (res == false)
                    log.trace("FAIL START THREAD: %s", text(tid_idx));
                else
                    log.trace("START THREAD IS SUCCESS: %s", text(tid_idx));
            });
    return res;
}

CRC32 hash;

/// serialize key2slot struct
public string serialize_key2slot(ref int[ string ] key2slot, out string hash_hex){
    OutBuffer outbuff = new OutBuffer();

    foreach (key, value; key2slot) {
        outbuff.write('"');
        outbuff.write(key);
        outbuff.write('"');
        outbuff.write(',');
        outbuff.write(text(value));
        outbuff.write('\n');
    }

    hash.start();
    hash.put(cast(ubyte[])outbuff.data);
    hash_hex = crcHexString(hash.finish());

    return outbuff.toString();
}

/// parse key2slot struct
public int[ string ] deserialize_key2slot(string data, out ResultCode rc){
    int[ string ] key2slot;
    rc = ResultCode.InternalServerError;

    int idx = 0;
    try
    {
        foreach (record; csvReader!(Tuple!(string, int))(data)) {
            if (record.length != 2) {
                stderr.writeln("ERR! key2slot, invalid record=", record);
                rc = ResultCode.UnprocessableEntity;
                return key2slot;
            }
            //stderr.writeln ("@&2 record=[", record, "]");

            if (idx > 0)
                key2slot[ record[ 0 ] ] = record[ 1 ];
            idx++;
        }
        rc = ResultCode.Ok;
    }
    catch (Throwable tr)
    {
        stderr.writeln("ERR! key2slot, row=%d, err=", idx, tr.msg);
        rc = ResultCode.UnprocessableEntity;
    }

    return key2slot;
}

string timeToString(long tm){
    SysTime sysTime = SysTime(tm);

    return sysTime.toISOExtString();
}

string timeToString(SysTime sysTime){
    return sysTime.toISOExtString();
}

long stringToTime(string str){
    try
    {
        if (str.length == 28) {
            str = str[ 0..23 ];
        }

        SysTime st = SysTime.fromISOExtString(str);
        return st.stdTime;
    }
    catch (Exception ex)
    {
        return 0;
    }
}

string fromStringz(char *s, int len){
    return cast(string)(s ? s[ 0 .. len ] : null);
}

// !!! stupid, but quickly
void formattedWrite(Writer, Char, A) (Writer w, in Char[] fmt, A[] args){
    if (args.length == 1) {
        std.format.formattedWrite(w, fmt, args[ 0 ]);
        return;
    }else if (args.length == 2) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ]);
        return;
    }else if (args.length == 3) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ]);
        return;
    }else if (args.length == 4) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ]);
        return;
    }else if (args.length == 5) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ]);
        return;
    }else if (args.length == 6) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ]);
        return;
    }else if (args.length == 7) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ]);
        return;
    }else if (args.length == 8) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ]);
        return;
    }else if (args.length == 9) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ],
                                  args[ 8 ]);
        return;
    }else if (args.length == 10) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ]);
        return;
    }else if (args.length == 11) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ]);
        return;
    }else if (args.length == 12) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ]);
        return;
    }else if (args.length == 13) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ]);
        return;
    }else if (args.length == 14) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ]);
        return;
    }else if (args.length == 15) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ], args[ 14 ]);
        return;
    }else if (args.length == 16) {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ], args[ 14 ], args[ 15 ]);
        return;
    }

    throw new Exception("util.formattedWrite (), count args > 16");
}

public JSONValue[] get_array(JSONValue jv, string field_name){
    if (field_name in jv.object) {
        return jv.object[ field_name ].array;
    }
    return null;
}

public string get_str(JSONValue jv, string field_name){
    if (field_name in jv.object) {
        return jv.object[ field_name ].str;
    }
    return null;
}

public long get_int(JSONValue jv, string field_name){
    if (field_name in jv.object) {
        return jv.object[ field_name ].integer;
    }
    return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * возвращает локальное время
 *
 * Returns: структура tm
 */
public tm *get_local_time(){
    time_t rawtime;
    tm     *timeinfo;

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    return timeinfo;
}

public string get_year(tm *timeinfo){
    return text(timeinfo.tm_year + 1900);
}

public string get_month(tm *timeinfo){
    if (timeinfo.tm_mon < 9)
        return "0" ~ text(timeinfo.tm_mon + 1);
    else
        return text(timeinfo.tm_mon + 1);
}

public string get_day(tm *timeinfo){
    if (timeinfo.tm_mday < 10)
        return "0" ~ text(timeinfo.tm_mday);
    else
        return text(timeinfo.tm_mday);
}

string to_lower_and_replace_delimeters(string in_text){
    if (in_text is null || in_text.length == 0)
        return in_text;

    char[] out_text = new char[ in_text.length ];

    for (int i = 0; i < in_text.length; i++) {
        char cc = in_text[ i ];
        if (cc == ':' || /*cc == ' ' ||*/ cc == '-')
            out_text[ i ] = '_';
        else
            out_text[ i ] = std.ascii.toLower(cc);
    }

    return out_text.idup;
}

